;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the strings library
;;;Date: Sat Jun 26, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009-2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (vicare)
  (vicare containers strings)
  (vicare containers char-sets)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare containers: high level strings library\n")


(parameterise ((check-test-name 'views))

  (check
      (substring* "ciao")
    => "ciao")

;;; --------------------------------------------------------------------

  (check
      (substring* (view "ciao"))
    => "ciao")

  (check
      (substring* (view "ciao" (start 2)))
    => "ao")

  (check
      (substring* (view "ciao" (start 0) (past 4)))
    => "ciao")

  (check
      (substring* (view "ciao" (start 0) (past 0)))
    => "")

  (check
      (substring* (view "ciao" (start 1) (past 1)))
    => "")

  (check
      (substring* (view "ciao" (start 0) (past 1)))
    => "c")

  (check
      (substring* (view "ciao" (past 2)))
    => "ci")

;;; --------------------------------------------------------------------

  (check
      (substring* (view "ciao" (start -2)))
    => "ao")

  (check
      (substring* (view "ciao" (past -1)))
    => "cia")

  (check
      (substring* (view "ciao" (past -2)))
    => "ci")

  (check
      (substring* (view "ciao" (start -3) (past -2)))
    => "i")

  #f)


(parameterise ((check-test-name 'constructors))

  (check
      (string-append "0123")
    => "0123")

  (check
      (string-append "0123" "45678")
    => "012345678")

  (check
      (string-append "")
    => "")

  (check
      (string-append "" "")
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-tabulate (lambda (idx) (integer->char (+ 65 idx))) 4)
    => "ABCD")

  (check
      (string-tabulate integer->char 0)
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-concatenate '("ciao" " " "hello" " " "salut"))
    => "ciao hello salut")

  (check
      (string-concatenate '())
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-concatenate-reverse '("ciao" " " "hello" " " "salut") " hola" 3)
    => "salut hello ciao ho")

  (check
      (string-concatenate-reverse '("ciao" " " "hello" " " "salut") " hola")
    => "salut hello ciao hola")

  (check
      (string-concatenate-reverse '("ciao" " " "hello" " " "salut"))
    => "salut hello ciao")

  (check
      (string-concatenate-reverse '())
    => "")

  )


(parameterise ((check-test-name 'predicates))

  (check
      (string-null? "ciao")
    => #f)

  (check
      (string-null? "")
    => #t)

;;; --------------------------------------------------------------------

  (check
      (guard (exc ((assertion-violation? exc)
		   (condition-who exc)))
	(string-every 123 "abc"))
    => '%string-every)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa"))
	(string-every #\a str))
    => #t)

  (check
      (let* ((str "aaaab"))
	(string-every #\a str))
    => #f)

  (check
      (let* ((str "aabaa"))
	(string-every #\a str))
    => #f)

  (check
      (let* ((str ""))
	(string-every #\a str))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa"))
	(string-every (char-set #\a) str))
    => #t)

  (check
      (let* ((str "aaaab"))
	(string-every (char-set #\a) str))
    => #f)

  (check
      (let* ((str "aabaa"))
	(string-every (char-set #\a) str))
    => #f)

  (check
      (let* ((str ""))
	(string-every (char-set #\a) str))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa"))
	(string-every char-alphabetic? str))
    => #t)

  (check
      (let* ((str "aaaa2"))
	(string-every char-alphabetic? str))
    => #f)

  (check
      (let* ((str "aa2aa"))
	(string-every char-alphabetic? str))
    => #f)

  (check
      (let* ((str ""))
	(string-every char-alphabetic? str))
    => #f)

  (check
      (let* ((str "1234"))
	(string-every (lambda (x) x) str))
    => #\4)

;;; --------------------------------------------------------------------

  (check
      (guard (exc ((assertion-violation? exc)
		   (condition-who exc)))
	(string-any 123 "abc"))
    => '%string-any)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "ddadd"))
	(string-any #\a str))
    => #t)

  (check
      (let* ((str "dddda"))
	(string-any #\a str))
    => #t)

  (check
      (let* ((str "ddd"))
	(string-any #\a str))
    => #f)

  (check
      (let* ((str ""))
	(string-any #\a str))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "dddaddd"))
	(string-any (char-set #\a) str))
    => #t)

  (check
      (let* ((str "ddda"))
	(string-any (char-set #\a) str))
    => #t)

  (check
      (let* ((str "dddd"))
	(string-any (char-set #\a) str))
    => #f)

  (check
      (let* ((str ""))
	(string-any (char-set #\a) str))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "11a11"))
	(string-any char-alphabetic? str))
    => #t)

  (check
      (let* ((str "11111a"))
	(string-any char-alphabetic? str))
    => #t)

  (check
      (let* ((str "1111"))
	(string-any char-alphabetic? str))
    => #f)

  (check
      (let* ((str ""))
	(string-any char-alphabetic? str))
    => #f)

  (check
      (let* ((str "1234"))
	(string-any (lambda (x) x) str))
    => #\1)

  )


(parameterise ((check-test-name 'comparison-case-sensitive))

  (check
      (string-compare "abcdefg" "abcd123" values values values)
    => 4)

  (check
      (string-compare "abcdef" "abcd123" values values values)
    => 4)

  (check
      (string-compare "efg" "123" values values values)
    => 0)

  (check
      (string-compare "" "abcd" values values values)
    => 0)

  (check
      (string-compare "abcd" "" values values values)
    => 0)

  (check
      (string-compare "abcdA" "abcdA"
		      (lambda (idx) 'less)
		      (lambda (idx) 'equal)
		      (lambda (idx) 'greater))
    => 'equal)

  (check
      (string-compare "abcdA" "abcdB"
		      (lambda (idx) 'less)
		      (lambda (idx) 'equal)
		      (lambda (idx) 'greater))
    => 'less)

  (check
      (string-compare "abcdB" "abcdA"
		      (lambda (idx) 'less)
		      (lambda (idx) 'equal)
		      (lambda (idx) 'greater))
    => 'greater)

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((str "abcd"))
     (string= str str)))

  (check-for-true
   (string= (view "12abcd" (start 2)) "abcd"))

  (check-for-false
   (string= "abc" "abcd"))

  (check-for-false
   (string= "abcd" "abc"))

  (check-for-false
   (string= "ABcd" "abcd"))

  (check-for-false
   (string= "abcd" "a2cd"))

;;; --------------------------------------------------------------------

  (check-for-false
   (string<> "abcd" "abcd"))

  (check-for-true
   (string<> "abc" "abcd"))

  (check-for-true
   (string<> "abcd" "abc"))

  (check-for-true
   (string<> "ABcd" "abcd"))

  (check-for-true
   (string<> "abcd" "a2cd"))

;;; --------------------------------------------------------------------

  (check-for-false
   (string< "abcd" "abcd"))

  (check-for-true
   (string< "abc" "abcd"))

  (check-for-false
   (string< "abcd" "abc"))

  (check-for-true
   (string< "ABcd" "abcd"))

  (check-for-false
   (string< "abcd" "a2cd"))

;;; --------------------------------------------------------------------

  (check-for-true
   (string<= "abcd" "abcd"))

  (check-for-true
   (string<= "abc" "abcd"))

  (check-for-false
   (string<= "abcd" "abc"))

  (check-for-true
   (string<= "ABcd" "abcd"))

  (check-for-false
   (string<= "abcd" "a2cd"))

;;; --------------------------------------------------------------------

  (check-for-false
   (string> "abcd" "abcd"))

  (check-for-true
   (string> "abcd" "abc"))

  (check-for-false
   (string> "abc" "abcd"))

  (check-for-true
   (string> "abcd" "ABcd"))

  (check-for-false
   (string> "a2cd" "abcd"))

;;; --------------------------------------------------------------------

  (check-for-true
   (string>= "abcd" "abcd"))

  (check-for-true
   (string>= "abcd" "abc"))

  (check-for-false
   (string>= "abc" "abcd"))

  (check-for-true
   (string>= "abcd" "ABcd"))

  (check-for-false
   (string>= "a2cd" "abcd"))

  )


(parameterise ((check-test-name 'comparison-case-insensitive))

  (check
      (string-compare-ci "aBcdefg" "abcd123" values values values)
    => 4)

  (check
      (string-compare-ci "efg" "123" values values values)
    => 0)

  (check
      (string-compare-ci "" "abcd" values values values)
    => 0)

  (check
      (string-compare-ci "abcd" "" values values values)
    => 0)

  (check
      (string-compare-ci "abcdA" "abcda"
			 (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater))
    => 'equal)

  (check
      (string-compare-ci "abcdA" "abcdb"
			 (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater))
    => 'less)

  (check
      (string-compare-ci "abcdb" "abcdA"
			 (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater))
    => 'greater)

;;; --------------------------------------------------------------------

  (check-for-true
   (string-ci= "abcd" "abcd"))

  (check-for-true
   (string-ci= (view "12abcd" (start 2)) "abcd"))

  (check-for-false
   (string-ci= "abc" "abcd"))

  (check-for-false
   (string-ci= "abcd" "abc"))

  (check-for-true
   (string-ci= "ABcd" "abcd"))

  (check-for-false
   (string-ci= "abcd" "a2cd"))

;;; --------------------------------------------------------------------

  (check-for-false
   (string-ci<> "abcd" "abcd"))

  (check-for-true
   (string-ci<> "abc" "abcd"))

  (check-for-true
   (string-ci<> "abcd" "abc"))

  (check-for-false
   (string-ci<> "ABcd" "abcd"))

  (check-for-true
   (string-ci<> "abcd" "a2cd"))

;;; --------------------------------------------------------------------

  (check-for-false
   (string-ci< "abcd" "abcd"))

  (check-for-true
   (string-ci< "abc" "abcd"))

  (check-for-false
   (string-ci< "abcd" "abc"))

  (check-for-false
   (string-ci< "ABcd" "abcd"))

  (check-for-false
   (string-ci< "abcd" "a2cd"))

;;; --------------------------------------------------------------------

  (check-for-true
   (string-ci<= "abcd" "abcd"))

  (check-for-true
   (string-ci<= "abc" "abcd"))

  (check-for-false
   (string-ci<= "abcd" "abc"))

  (check-for-true
   (string-ci<= "ABcd" "abcd"))

  (check-for-false
   (string-ci<= "abcd" "a2cd"))

;;; --------------------------------------------------------------------

  (check-for-false
   (string-ci> "abcd" "abcd"))

  (check-for-true
   (string-ci> "abcd" "abc"))

  (check-for-false
   (string-ci> "abc" "abcd"))

  (check-for-false
   (string-ci> "abcd" "ABcd"))

  (check-for-false
   (string-ci> "a2cd" "abcd"))

;;; --------------------------------------------------------------------

  (check-for-true
   (string-ci>= "abcd" "abcd"))

  (check-for-true
   (string-ci>= "abcd" "abc"))

  (check-for-false
   (string-ci>= "abc" "abcd"))

  (check-for-true
   (string-ci>= "abcd" "ABcd"))

  (check-for-false
   (string-ci>= "a2cd" "abcd"))

  #t)


(parameterise ((check-test-name 'comparison-dictionary-case-sensitive))

  (check (string-dictionary=? "" "")				=> #t)
  (check (string-dictionary=? "a" "")				=> #f)
  (check (string-dictionary=? "" "a")				=> #f)
  (check (string-dictionary=? "ab" "a")				=> #f)
  (check (string-dictionary=? "a" "ab")				=> #f)
  (check (string-dictionary=? "ciao" "ciao")			=> #t)
  (check (string-dictionary=? "ciao1" "ciao")			=> #f)
  (check (string-dictionary=? "ciao" "ciao1")			=> #f)

  (check (string-dictionary=? "ci ao" "ciao")			=> #t)
  (check (string-dictionary=? "ciao" "ci ao")			=> #t)
  (check (string-dictionary=? "ci\tao" "ciao")			=> #t)
  (check (string-dictionary=? "ciao" "ci\tao")			=> #t)
  (check (string-dictionary=? "ci\nao" "ciao")			=> #t)
  (check (string-dictionary=? "ciao" "ci\nao")			=> #t)
  (check (string-dictionary=? "ci\vao" "ciao")			=> #t)
  (check (string-dictionary=? "ciao" "ci\tao")			=> #t)
  (check (string-dictionary=? "ci\fao" "ciao")			=> #t)
  (check (string-dictionary=? "ciao" "ci\fao")			=> #t)
  (check (string-dictionary=? "ci\rao" "ciao")			=> #t)
  (check (string-dictionary=? "ciao" "ci\rao")			=> #t)

;;; --------------------------------------------------------------------

  (check (string-dictionary<? "" "")				=> #f)
  (check (string-dictionary<? "a" "")				=> #f)
  (check (string-dictionary<? "" "a")				=> #t)
  (check (string-dictionary<? "ab" "a")				=> #f)
  (check (string-dictionary<? "a" "ab")				=> #t)
  (check (string-dictionary<? "ciao" "ciao")			=> #f)
  (check (string-dictionary<? "ciao1" "ciao")			=> #f)
  (check (string-dictionary<? "ciao" "ciao1")			=> #t)

  (check (string-dictionary<? "ci ao" "ciao")			=> #f)
  (check (string-dictionary<? "ciao" "ci ao")			=> #f)
  (check (string-dictionary<? "ci\tao" "ciao")			=> #f)
  (check (string-dictionary<? "ciao" "ci\tao")			=> #f)
  (check (string-dictionary<? "ci\nao" "ciao")			=> #f)
  (check (string-dictionary<? "ciao" "ci\nao")			=> #f)
  (check (string-dictionary<? "ci\vao" "ciao")			=> #f)
  (check (string-dictionary<? "ciao" "ci\tao")			=> #f)
  (check (string-dictionary<? "ci\fao" "ciao")			=> #f)
  (check (string-dictionary<? "ciao" "ci\fao")			=> #f)
  (check (string-dictionary<? "ci\rao" "ciao")			=> #f)
  (check (string-dictionary<? "ciao" "ci\rao")			=> #f)

;;; --------------------------------------------------------------------

  (check (string-dictionary<=? "" "")				=> #t)
  (check (string-dictionary<=? "a" "")				=> #f)
  (check (string-dictionary<=? "" "a")				=> #t)
  (check (string-dictionary<=? "ab" "a")			=> #f)
  (check (string-dictionary<=? "a" "ab")			=> #t)
  (check (string-dictionary<=? "ciao" "ciao")			=> #t)
  (check (string-dictionary<=? "ciao1" "ciao")			=> #f)
  (check (string-dictionary<=? "ciao" "ciao1")			=> #t)

  (check (string-dictionary<=? "ci ao" "ciao")			=> #t)
  (check (string-dictionary<=? "ciao" "ci ao")			=> #t)
  (check (string-dictionary<=? "ci\tao" "ciao")			=> #t)
  (check (string-dictionary<=? "ciao" "ci\tao")			=> #t)
  (check (string-dictionary<=? "ci\nao" "ciao")			=> #t)
  (check (string-dictionary<=? "ciao" "ci\nao")			=> #t)
  (check (string-dictionary<=? "ci\vao" "ciao")			=> #t)
  (check (string-dictionary<=? "ciao" "ci\tao")			=> #t)
  (check (string-dictionary<=? "ci\fao" "ciao")			=> #t)
  (check (string-dictionary<=? "ciao" "ci\fao")			=> #t)
  (check (string-dictionary<=? "ci\rao" "ciao")			=> #t)
  (check (string-dictionary<=? "ciao" "ci\rao")			=> #t)

;;; --------------------------------------------------------------------

  (check (string-dictionary>? "" "")				=> #f)
  (check (string-dictionary>? "a" "")				=> #t)
  (check (string-dictionary>? "" "a")				=> #f)
  (check (string-dictionary>? "ab" "a")				=> #t)
  (check (string-dictionary>? "a" "ab")				=> #f)
  (check (string-dictionary>? "ciao" "ciao")			=> #f)
  (check (string-dictionary>? "ciao1" "ciao")			=> #t)
  (check (string-dictionary>? "ciao" "ciao1")			=> #f)

  (check (string-dictionary>? "ci ao" "ciao")			=> #f)
  (check (string-dictionary>? "ciao" "ci ao")			=> #f)
  (check (string-dictionary>? "ci\tao" "ciao")			=> #f)
  (check (string-dictionary>? "ciao" "ci\tao")			=> #f)
  (check (string-dictionary>? "ci\nao" "ciao")			=> #f)
  (check (string-dictionary>? "ciao" "ci\nao")			=> #f)
  (check (string-dictionary>? "ci\vao" "ciao")			=> #f)
  (check (string-dictionary>? "ciao" "ci\tao")			=> #f)
  (check (string-dictionary>? "ci\fao" "ciao")			=> #f)
  (check (string-dictionary>? "ciao" "ci\fao")			=> #f)
  (check (string-dictionary>? "ci\rao" "ciao")			=> #f)
  (check (string-dictionary>? "ciao" "ci\rao")			=> #f)

;;; --------------------------------------------------------------------

  (check (string-dictionary>=? "" "")				=> #t)
  (check (string-dictionary>=? "a" "")				=> #t)
  (check (string-dictionary>=? "" "a")				=> #f)
  (check (string-dictionary>=? "ab" "a")			=> #t)
  (check (string-dictionary>=? "a" "ab")			=> #f)
  (check (string-dictionary>=? "ciao" "ciao")			=> #t)
  (check (string-dictionary>=? "ciao1" "ciao")			=> #t)
  (check (string-dictionary>=? "ciao" "ciao1")			=> #f)

  (check (string-dictionary>=? "ci ao" "ciao")			=> #t)
  (check (string-dictionary>=? "ciao" "ci ao")			=> #t)
  (check (string-dictionary>=? "ci\tao" "ciao")			=> #t)
  (check (string-dictionary>=? "ciao" "ci\tao")			=> #t)
  (check (string-dictionary>=? "ci\nao" "ciao")			=> #t)
  (check (string-dictionary>=? "ciao" "ci\nao")			=> #t)
  (check (string-dictionary>=? "ci\vao" "ciao")			=> #t)
  (check (string-dictionary>=? "ciao" "ci\tao")			=> #t)
  (check (string-dictionary>=? "ci\fao" "ciao")			=> #t)
  (check (string-dictionary>=? "ciao" "ci\fao")			=> #t)
  (check (string-dictionary>=? "ci\rao" "ciao")			=> #t)
  (check (string-dictionary>=? "ciao" "ci\rao")			=> #t)

  #t)


(parameterise ((check-test-name 'comparison-dictionary-case-insensitive))

  (check (string-dictionary-compare-ci "" "")			=> 0)
  (check (string-dictionary-compare-ci "a" "")			=> +1)
  (check (string-dictionary-compare-ci "" "a")			=> -1)
  (check (string-dictionary-compare-ci "ab" "a")		=> +1)
  (check (string-dictionary-compare-ci "a" "ab")		=> -1)
  (check (string-dictionary-compare-ci "ciao" "ciao")		=> 0)
  (check (string-dictionary-compare-ci "ciao1" "ciao")		=> +1)
  (check (string-dictionary-compare-ci "ciao" "ciao1")		=> -1)
  (check (string-dictionary-compare-ci "CIAO" "ciao")		=> 0)
  (check (string-dictionary-compare-ci "CIAO1" "ciao")		=> +1)
  (check (string-dictionary-compare-ci "CIAO" "ciao1")		=> -1)

  (check (string-dictionary-compare-ci "ci ao" "ciao")		=> 0)
  (check (string-dictionary-compare-ci "ciao" "ci ao")		=> 0)
  (check (string-dictionary-compare-ci "ci\tao" "ciao")		=> 0)
  (check (string-dictionary-compare-ci "ciao" "ci\tao")		=> 0)
  (check (string-dictionary-compare-ci "ci\nao" "ciao")		=> 0)
  (check (string-dictionary-compare-ci "ciao" "ci\nao")		=> 0)
  (check (string-dictionary-compare-ci "ci\vao" "ciao")		=> 0)
  (check (string-dictionary-compare-ci "ciao" "ci\tao")		=> 0)
  (check (string-dictionary-compare-ci "ci\fao" "ciao")		=> 0)
  (check (string-dictionary-compare-ci "ciao" "ci\fao")		=> 0)
  (check (string-dictionary-compare-ci "ci\rao" "ciao")		=> 0)
  (check (string-dictionary-compare-ci "ciao" "ci\rao")		=> 0)

;;; --------------------------------------------------------------------

  (check (string-dictionary-ci=? "" "")				=> #t)
  (check (string-dictionary-ci=? "a" "")			=> #f)
  (check (string-dictionary-ci=? "" "a")			=> #f)
  (check (string-dictionary-ci=? "ab" "a")			=> #f)
  (check (string-dictionary-ci=? "a" "ab")			=> #f)
  (check (string-dictionary-ci=? "ciao" "ciao")			=> #t)
  (check (string-dictionary-ci=? "ciao1" "ciao")		=> #f)
  (check (string-dictionary-ci=? "ciao" "ciao1")		=> #f)
  (check (string-dictionary-ci=? "CIAO" "ciao")			=> #t)
  (check (string-dictionary-ci=? "CIAO1" "ciao")		=> #f)
  (check (string-dictionary-ci=? "CIAO" "ciao1")		=> #f)

  (check (string-dictionary-ci=? "ci ao" "ciao")		=> #t)
  (check (string-dictionary-ci=? "ciao" "ci ao")		=> #t)
  (check (string-dictionary-ci=? "ci\tao" "ciao")		=> #t)
  (check (string-dictionary-ci=? "ciao" "ci\tao")		=> #t)
  (check (string-dictionary-ci=? "ci\nao" "ciao")		=> #t)
  (check (string-dictionary-ci=? "ciao" "ci\nao")		=> #t)
  (check (string-dictionary-ci=? "ci\vao" "ciao")		=> #t)
  (check (string-dictionary-ci=? "ciao" "ci\tao")		=> #t)
  (check (string-dictionary-ci=? "ci\fao" "ciao")		=> #t)
  (check (string-dictionary-ci=? "ciao" "ci\fao")		=> #t)
  (check (string-dictionary-ci=? "ci\rao" "ciao")		=> #t)
  (check (string-dictionary-ci=? "ciao" "ci\rao")		=> #t)

;;; --------------------------------------------------------------------

  (check (string-dictionary-ci<? "" "")				=> #f)
  (check (string-dictionary-ci<? "a" "")			=> #f)
  (check (string-dictionary-ci<? "" "a")			=> #t)
  (check (string-dictionary-ci<? "ab" "a")			=> #f)
  (check (string-dictionary-ci<? "a" "ab")			=> #t)
  (check (string-dictionary-ci<? "ciao" "ciao")			=> #f)
  (check (string-dictionary-ci<? "ciao1" "ciao")		=> #f)
  (check (string-dictionary-ci<? "ciao" "ciao1")		=> #t)
  (check (string-dictionary-ci<? "CIAO" "ciao")			=> #f)
  (check (string-dictionary-ci<? "CIAO1" "ciao")		=> #f)
  (check (string-dictionary-ci<? "CIAO" "ciao1")		=> #t)

  (check (string-dictionary-ci<? "ci ao" "ciao")		=> #f)
  (check (string-dictionary-ci<? "ciao" "ci ao")		=> #f)
  (check (string-dictionary-ci<? "ci\tao" "ciao")		=> #f)
  (check (string-dictionary-ci<? "ciao" "ci\tao")		=> #f)
  (check (string-dictionary-ci<? "ci\nao" "ciao")		=> #f)
  (check (string-dictionary-ci<? "ciao" "ci\nao")		=> #f)
  (check (string-dictionary-ci<? "ci\vao" "ciao")		=> #f)
  (check (string-dictionary-ci<? "ciao" "ci\tao")		=> #f)
  (check (string-dictionary-ci<? "ci\fao" "ciao")		=> #f)
  (check (string-dictionary-ci<? "ciao" "ci\fao")		=> #f)
  (check (string-dictionary-ci<? "ci\rao" "ciao")		=> #f)
  (check (string-dictionary-ci<? "ciao" "ci\rao")		=> #f)

;;; --------------------------------------------------------------------

  (check (string-dictionary-ci<=? "" "")			=> #t)
  (check (string-dictionary-ci<=? "a" "")			=> #f)
  (check (string-dictionary-ci<=? "" "a")			=> #t)
  (check (string-dictionary-ci<=? "ab" "a")			=> #f)
  (check (string-dictionary-ci<=? "a" "ab")			=> #t)
  (check (string-dictionary-ci<=? "ciao" "ciao")		=> #t)
  (check (string-dictionary-ci<=? "ciao1" "ciao")		=> #f)
  (check (string-dictionary-ci<=? "ciao" "ciao1")		=> #t)
  (check (string-dictionary-ci<=? "CIAO" "ciao")		=> #t)
  (check (string-dictionary-ci<=? "CIAO1" "ciao")		=> #f)
  (check (string-dictionary-ci<=? "CIAO" "ciao1")		=> #t)

  (check (string-dictionary-ci<=? "ci ao" "ciao")		=> #t)
  (check (string-dictionary-ci<=? "ciao" "ci ao")		=> #t)
  (check (string-dictionary-ci<=? "ci\tao" "ciao")		=> #t)
  (check (string-dictionary-ci<=? "ciao" "ci\tao")		=> #t)
  (check (string-dictionary-ci<=? "ci\nao" "ciao")		=> #t)
  (check (string-dictionary-ci<=? "ciao" "ci\nao")		=> #t)
  (check (string-dictionary-ci<=? "ci\vao" "ciao")		=> #t)
  (check (string-dictionary-ci<=? "ciao" "ci\tao")		=> #t)
  (check (string-dictionary-ci<=? "ci\fao" "ciao")		=> #t)
  (check (string-dictionary-ci<=? "ciao" "ci\fao")		=> #t)
  (check (string-dictionary-ci<=? "ci\rao" "ciao")		=> #t)
  (check (string-dictionary-ci<=? "ciao" "ci\rao")		=> #t)

;;; --------------------------------------------------------------------

  (check (string-dictionary-ci>? "" "")				=> #f)
  (check (string-dictionary-ci>? "a" "")			=> #t)
  (check (string-dictionary-ci>? "" "a")			=> #f)
  (check (string-dictionary-ci>? "ab" "a")			=> #t)
  (check (string-dictionary-ci>? "a" "ab")			=> #f)
  (check (string-dictionary-ci>? "ciao" "ciao")			=> #f)
  (check (string-dictionary-ci>? "ciao1" "ciao")		=> #t)
  (check (string-dictionary-ci>? "ciao" "ciao1")		=> #f)
  (check (string-dictionary-ci>? "CIAO" "ciao")			=> #f)
  (check (string-dictionary-ci>? "CIAO1" "ciao")		=> #t)
  (check (string-dictionary-ci>? "CIAO" "ciao1")		=> #f)

  (check (string-dictionary-ci>? "ci ao" "ciao")		=> #f)
  (check (string-dictionary-ci>? "ciao" "ci ao")		=> #f)
  (check (string-dictionary-ci>? "ci\tao" "ciao")		=> #f)
  (check (string-dictionary-ci>? "ciao" "ci\tao")		=> #f)
  (check (string-dictionary-ci>? "ci\nao" "ciao")		=> #f)
  (check (string-dictionary-ci>? "ciao" "ci\nao")		=> #f)
  (check (string-dictionary-ci>? "ci\vao" "ciao")		=> #f)
  (check (string-dictionary-ci>? "ciao" "ci\tao")		=> #f)
  (check (string-dictionary-ci>? "ci\fao" "ciao")		=> #f)
  (check (string-dictionary-ci>? "ciao" "ci\fao")		=> #f)
  (check (string-dictionary-ci>? "ci\rao" "ciao")		=> #f)
  (check (string-dictionary-ci>? "ciao" "ci\rao")		=> #f)

;;; --------------------------------------------------------------------

  (check (string-dictionary-ci>=? "" "")			=> #t)
  (check (string-dictionary-ci>=? "a" "")			=> #t)
  (check (string-dictionary-ci>=? "" "a")			=> #f)
  (check (string-dictionary-ci>=? "ab" "a")			=> #t)
  (check (string-dictionary-ci>=? "a" "ab")			=> #f)
  (check (string-dictionary-ci>=? "ciao" "ciao")		=> #t)
  (check (string-dictionary-ci>=? "ciao1" "ciao")		=> #t)
  (check (string-dictionary-ci>=? "ciao" "ciao1")		=> #f)
  (check (string-dictionary-ci>=? "CIAO" "ciao")		=> #t)
  (check (string-dictionary-ci>=? "CIAO1" "ciao")		=> #t)
  (check (string-dictionary-ci>=? "CIAO" "ciao1")		=> #f)

  (check (string-dictionary-ci>=? "ci ao" "ciao")		=> #t)
  (check (string-dictionary-ci>=? "ciao" "ci ao")		=> #t)
  (check (string-dictionary-ci>=? "ci\tao" "ciao")		=> #t)
  (check (string-dictionary-ci>=? "ciao" "ci\tao")		=> #t)
  (check (string-dictionary-ci>=? "ci\nao" "ciao")		=> #t)
  (check (string-dictionary-ci>=? "ciao" "ci\nao")		=> #t)
  (check (string-dictionary-ci>=? "ci\vao" "ciao")		=> #t)
  (check (string-dictionary-ci>=? "ciao" "ci\tao")		=> #t)
  (check (string-dictionary-ci>=? "ci\fao" "ciao")		=> #t)
  (check (string-dictionary-ci>=? "ciao" "ci\fao")		=> #t)
  (check (string-dictionary-ci>=? "ci\rao" "ciao")		=> #t)
  (check (string-dictionary-ci>=? "ciao" "ci\rao")		=> #t)

  #t)


(parameterise ((check-test-name 'comparison-string/number-case-sensitive))

  (check (string/numbers=? "" "")				=> #t)
  (check (string/numbers=? "a" "")				=> #f)
  (check (string/numbers=? "" "a")				=> #f)
  (check (string/numbers=? "a" "a")				=> #t)
  (check (string/numbers=? "1" "")				=> #f)
  (check (string/numbers=? "" "1")				=> #f)
  (check (string/numbers=? "1" "1")				=> #t)
  (check (string/numbers=? "1" "2")				=> #f)
  (check (string/numbers=? "2" "1")				=> #f)
  (check (string/numbers=? "a" "ab")				=> #f)
  (check (string/numbers=? "ab" "a")				=> #f)
  (check (string/numbers=? "a" "a1")				=> #f)
  (check (string/numbers=? "a1" "a")				=> #f)
  (check (string/numbers=? "1" "1a")				=> #f)
  (check (string/numbers=? "1a" "1")				=> #f)

  (check (string/numbers=? "123" "45")				=> #f)
  (check (string/numbers=? "45" "123")				=> #f)
  (check (string/numbers=? "ciao3" "ciao10")			=> #f)
  (check (string/numbers=? "ciao10" "ciao3")			=> #f)
  (check (string/numbers=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (string/numbers=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (string/numbers=? "foo4bar3zab" "foo4bar10")		=> #f)
  (check (string/numbers=? "foo4bar10" "foo4bar3zab")		=> #f)
  (check (string/numbers=? "foo12" "12foo")			=> #f)
  (check (string/numbers=? "12foo" "foo12")			=> #f)
  (check (string/numbers=? "12bar" "foobar")			=> #f)
  (check (string/numbers=? "12.3" "12.3")			=> #t)
  (check (string/numbers=? "12.3" "12.10")			=> #f)
  (check (string/numbers=? "12.10" "12.3")			=> #f)
  (check (string/numbers=? "12.3" "12,10")			=> #f)
  (check (string/numbers=? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check (string/numbers<>? "" "")				=> #f)
  (check (string/numbers<>? "a" "")				=> #t)
  (check (string/numbers<>? "" "a")				=> #t)
  (check (string/numbers<>? "a" "a")				=> #f)
  (check (string/numbers<>? "1" "")				=> #t)
  (check (string/numbers<>? "" "1")				=> #t)
  (check (string/numbers<>? "1" "1")				=> #f)
  (check (string/numbers<>? "1" "2")				=> #t)
  (check (string/numbers<>? "2" "1")				=> #t)
  (check (string/numbers<>? "a" "ab")				=> #t)
  (check (string/numbers<>? "ab" "a")				=> #t)
  (check (string/numbers<>? "a" "a1")				=> #t)
  (check (string/numbers<>? "a1" "a")				=> #t)
  (check (string/numbers<>? "1" "1a")				=> #t)
  (check (string/numbers<>? "1a" "1")				=> #t)

  (check (string/numbers<>? "123" "45")				=> #t)
  (check (string/numbers<>? "45" "123")				=> #t)
  (check (string/numbers<>? "ciao3" "ciao10")			=> #t)
  (check (string/numbers<>? "ciao10" "ciao3")			=> #t)
  (check (string/numbers<>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (string/numbers<>? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (string/numbers<>? "foo4bar3zab" "foo4bar10")		=> #t)
  (check (string/numbers<>? "foo4bar10" "foo4bar3zab")		=> #t)
  (check (string/numbers<>? "foo12" "12foo")			=> #t)
  (check (string/numbers<>? "12foo" "foo12")			=> #t)
  (check (string/numbers<>? "12bar" "foobar")			=> #t)
  (check (string/numbers<>? "12.3" "12.3")			=> #f)
  (check (string/numbers<>? "12.3" "12.10")			=> #t)
  (check (string/numbers<>? "12.10" "12.3")			=> #t)
  (check (string/numbers<>? "12.3" "12,10")			=> #t)
  (check (string/numbers<>? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (string/numbers<? "" "")				=> #f)
  (check (string/numbers<? "a" "")				=> #f)
  (check (string/numbers<? "" "a")				=> #t)
  (check (string/numbers<? "a" "a")				=> #f)
  (check (string/numbers<? "1" "")				=> #f)
  (check (string/numbers<? "" "1")				=> #t)
  (check (string/numbers<? "1" "1")				=> #f)
  (check (string/numbers<? "1" "2")				=> #t)
  (check (string/numbers<? "2" "1")				=> #f)
  (check (string/numbers<? "a" "ab")				=> #t)
  (check (string/numbers<? "ab" "a")				=> #f)
  (check (string/numbers<? "a" "a1")				=> #t)
  (check (string/numbers<? "a1" "a")				=> #f)
  (check (string/numbers<? "1" "1a")				=> #t)
  (check (string/numbers<? "1a" "1")				=> #f)

  (check (string/numbers<? "123" "45")				=> #f)
  (check (string/numbers<? "45" "123")				=> #t)
  (check (string/numbers<? "ciao3" "ciao10")			=> #t)
  (check (string/numbers<? "ciao10" "ciao3")			=> #f)
  (check (string/numbers<? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (string/numbers<? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (string/numbers<? "foo4bar3zab" "foo4bar10")		=> #t)
  (check (string/numbers<? "foo4bar10" "foo4bar3zab")		=> #f)
  (check (string/numbers<? "foo12" "12foo")			=> #f)
  (check (string/numbers<? "12foo" "foo12")			=> #t)
  (check (string/numbers<? "12bar" "foobar")			=> #t)
  (check (string/numbers<? "12.3" "12.3")			=> #f)
  (check (string/numbers<? "12.3" "12.10")			=> #t)
  (check (string/numbers<? "12.10" "12.3")			=> #f)
  (check (string/numbers<? "12.3" "12,10")			=> #f)
  (check (string/numbers<? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (string/numbers<=? "" "")				=> #t)
  (check (string/numbers<=? "a" "")				=> #f)
  (check (string/numbers<=? "" "a")				=> #t)
  (check (string/numbers<=? "a" "a")				=> #t)
  (check (string/numbers<=? "1" "")				=> #f)
  (check (string/numbers<=? "" "1")				=> #t)
  (check (string/numbers<=? "1" "1")				=> #t)
  (check (string/numbers<=? "1" "2")				=> #t)
  (check (string/numbers<=? "2" "1")				=> #f)
  (check (string/numbers<=? "a" "ab")				=> #t)
  (check (string/numbers<=? "ab" "a")				=> #f)
  (check (string/numbers<=? "a" "a1")				=> #t)
  (check (string/numbers<=? "a1" "a")				=> #f)
  (check (string/numbers<=? "1" "1a")				=> #t)
  (check (string/numbers<=? "1a" "1")				=> #f)

  (check (string/numbers<=? "123" "45")				=> #f)
  (check (string/numbers<=? "45" "123")				=> #t)
  (check (string/numbers<=? "ciao3" "ciao10")			=> #t)
  (check (string/numbers<=? "ciao10" "ciao3")			=> #f)
  (check (string/numbers<=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (string/numbers<=? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (string/numbers<=? "foo4bar3zab" "foo4bar10")		=> #t)
  (check (string/numbers<=? "foo4bar10" "foo4bar3zab")		=> #f)
  (check (string/numbers<=? "foo12" "12foo")			=> #f)
  (check (string/numbers<=? "12foo" "foo12")			=> #t)
  (check (string/numbers<=? "12bar" "foobar")			=> #t)
  (check (string/numbers<=? "12.3" "12.3")			=> #t)
  (check (string/numbers<=? "12.3" "12.10")			=> #t)
  (check (string/numbers<=? "12.10" "12.3")			=> #f)
  (check (string/numbers<=? "12.3" "12,10")			=> #f)
  (check (string/numbers<=? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (string/numbers>? "" "")				=> #f)
  (check (string/numbers>? "a" "")				=> #t)
  (check (string/numbers>? "" "a")				=> #f)
  (check (string/numbers>? "a" "a")				=> #f)
  (check (string/numbers>? "1" "")				=> #t)
  (check (string/numbers>? "" "1")				=> #f)
  (check (string/numbers>? "1" "1")				=> #f)
  (check (string/numbers>? "1" "2")				=> #f)
  (check (string/numbers>? "2" "1")				=> #t)
  (check (string/numbers>? "a" "ab")				=> #f)
  (check (string/numbers>? "ab" "a")				=> #t)
  (check (string/numbers>? "a" "a1")				=> #f)
  (check (string/numbers>? "a1" "a")				=> #t)
  (check (string/numbers>? "1" "1a")				=> #f)
  (check (string/numbers>? "1a" "1")				=> #t)

  (check (string/numbers>? "123" "45")				=> #t)
  (check (string/numbers>? "45" "123")				=> #f)
  (check (string/numbers>? "ciao3" "ciao10")			=> #f)
  (check (string/numbers>? "ciao10" "ciao3")			=> #t)
  (check (string/numbers>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (string/numbers>? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (string/numbers>? "foo4bar3zab" "foo4bar10")		=> #f)
  (check (string/numbers>? "foo4bar10" "foo4bar3zab")		=> #t)
  (check (string/numbers>? "foo12" "12foo")			=> #t)
  (check (string/numbers>? "12foo" "foo12")			=> #f)
  (check (string/numbers>? "12bar" "foobar")			=> #f)
  (check (string/numbers>? "12.3" "12.3")			=> #f)
  (check (string/numbers>? "12.3" "12.10")			=> #f)
  (check (string/numbers>? "12.10" "12.3")			=> #t)
  (check (string/numbers>? "12.3" "12,10")			=> #t)
  (check (string/numbers>? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check (string/numbers>=? "" "")				=> #t)
  (check (string/numbers>=? "a" "")				=> #t)
  (check (string/numbers>=? "" "a")				=> #f)
  (check (string/numbers>=? "a" "a")				=> #t)
  (check (string/numbers>=? "1" "")				=> #t)
  (check (string/numbers>=? "" "1")				=> #f)
  (check (string/numbers>=? "1" "1")				=> #t)
  (check (string/numbers>=? "1" "2")				=> #f)
  (check (string/numbers>=? "2" "1")				=> #t)
  (check (string/numbers>=? "a" "ab")				=> #f)
  (check (string/numbers>=? "ab" "a")				=> #t)
  (check (string/numbers>=? "a" "a1")				=> #f)
  (check (string/numbers>=? "a1" "a")				=> #t)
  (check (string/numbers>=? "1" "1a")				=> #f)
  (check (string/numbers>=? "1a" "1")				=> #t)

  (check (string/numbers>=? "123" "45")				=> #t)
  (check (string/numbers>=? "45" "123")				=> #f)
  (check (string/numbers>=? "ciao3" "ciao10")			=> #f)
  (check (string/numbers>=? "ciao10" "ciao3")			=> #t)
  (check (string/numbers>=? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (string/numbers>=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (string/numbers>=? "foo4bar3zab" "foo4bar10")		=> #f)
  (check (string/numbers>=? "foo4bar10" "foo4bar3zab")		=> #t)
  (check (string/numbers>=? "foo12" "12foo")			=> #t)
  (check (string/numbers>=? "12foo" "foo12")			=> #f)
  (check (string/numbers>=? "12bar" "foobar")			=> #f)
  (check (string/numbers>=? "12.3" "12.3")			=> #t)
  (check (string/numbers>=? "12.3" "12.10")			=> #f)
  (check (string/numbers>=? "12.10" "12.3")			=> #t)
  (check (string/numbers>=? "12.3" "12,10")			=> #t)
  (check (string/numbers>=? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check
      (list-sort string/numbers<? (quote ("foo123" "foo42" "foo7")))
    => '("foo7" "foo42" "foo123"))

  #t)


(parameterise ((check-test-name 'comparison-string/number-case-insensitive))

  (check (string/numbers-compare-ci "" "")			=> 0)
  (check (string/numbers-compare-ci "a" "")			=> +1)
  (check (string/numbers-compare-ci "" "a")			=> -1)
  (check (string/numbers-compare-ci "a" "a")			=> 0)
  (check (string/numbers-compare-ci "1" "")			=> +1)
  (check (string/numbers-compare-ci "" "1")			=> -1)
  (check (string/numbers-compare-ci "1" "1")			=> 0)
  (check (string/numbers-compare-ci "1" "2")			=> -1)
  (check (string/numbers-compare-ci "2" "1")			=> +1)
  (check (string/numbers-compare-ci "a" "ab")			=> -1)
  (check (string/numbers-compare-ci "ab" "a")			=> +1)
  (check (string/numbers-compare-ci "a" "a1")			=> -1)
  (check (string/numbers-compare-ci "a1" "a")			=> +1)
  (check (string/numbers-compare-ci "1" "1a")			=> -1)
  (check (string/numbers-compare-ci "1a" "1")			=> +1)
  (check (string/numbers-compare-ci "a" "A")			=> 0)
  (check (string/numbers-compare-ci "A" "a")			=> 0)

  (check (string/numbers-compare-ci "123" "45")			=> +1)
  (check (string/numbers-compare-ci "45" "123")			=> -1)
  (check (string/numbers-compare-ci "ciao3" "ciao10")		=> -1)
  (check (string/numbers-compare-ci "ciao10" "ciao3")		=> +1)
  (check (string/numbers-compare-ci "foo4bar3zab10" "foo4bar3zab2")	=> +1)
  (check (string/numbers-compare-ci "foo4bar3zab2" "foo4bar3zab10")	=> -1)
  (check (string/numbers-compare-ci "foo4bar3zab" "foo4bar10")	=> -1)
  (check (string/numbers-compare-ci "foo4bar10" "foo4bar3zab")	=> +1)
  (check (string/numbers-compare-ci "foo12" "12foo")		=> +1)
  (check (string/numbers-compare-ci "12foo" "foo12")		=> -1)
  (check (string/numbers-compare-ci "12bar" "foobar")		=> -1)
  (check (string/numbers-compare-ci "12.3" "12.3")		=> 0)
  (check (string/numbers-compare-ci "12.3" "12.10")		=> -1)
  (check (string/numbers-compare-ci "12.10" "12.3")		=> +1)
  (check (string/numbers-compare-ci "12.3" "12,10")		=> +1)
  (check (string/numbers-compare-ci "12,10" "12.3")		=> -1)

;;; --------------------------------------------------------------------

  (check (string/numbers-ci=? "" "")				=> #t)
  (check (string/numbers-ci=? "a" "")				=> #f)
  (check (string/numbers-ci=? "" "a")				=> #f)
  (check (string/numbers-ci=? "a" "a")				=> #t)
  (check (string/numbers-ci=? "1" "")				=> #f)
  (check (string/numbers-ci=? "" "1")				=> #f)
  (check (string/numbers-ci=? "1" "1")				=> #t)
  (check (string/numbers-ci=? "1" "2")				=> #f)
  (check (string/numbers-ci=? "2" "1")				=> #f)
  (check (string/numbers-ci=? "a" "ab")				=> #f)
  (check (string/numbers-ci=? "ab" "a")				=> #f)
  (check (string/numbers-ci=? "a" "a1")				=> #f)
  (check (string/numbers-ci=? "a1" "a")				=> #f)
  (check (string/numbers-ci=? "1" "1a")				=> #f)
  (check (string/numbers-ci=? "1a" "1")				=> #f)
  (check (string/numbers-ci=? "a" "A")				=> #t)
  (check (string/numbers-ci=? "A" "a")				=> #t)

  (check (string/numbers-ci=? "123" "45")			=> #f)
  (check (string/numbers-ci=? "45" "123")			=> #f)
  (check (string/numbers-ci=? "ciao3" "ciao10")			=> #f)
  (check (string/numbers-ci=? "ciao10" "ciao3")			=> #f)
  (check (string/numbers-ci=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (string/numbers-ci=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (string/numbers-ci=? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (string/numbers-ci=? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (string/numbers-ci=? "foo12" "12foo")			=> #f)
  (check (string/numbers-ci=? "12foo" "foo12")			=> #f)
  (check (string/numbers-ci=? "12bar" "foobar")			=> #f)
  (check (string/numbers-ci=? "12.3" "12.3")			=> #t)
  (check (string/numbers-ci=? "12.3" "12.10")			=> #f)
  (check (string/numbers-ci=? "12.10" "12.3")			=> #f)
  (check (string/numbers-ci=? "12.3" "12,10")			=> #f)
  (check (string/numbers-ci=? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check (string/numbers-ci<>? "" "")				=> #f)
  (check (string/numbers-ci<>? "a" "")				=> #t)
  (check (string/numbers-ci<>? "" "a")				=> #t)
  (check (string/numbers-ci<>? "a" "a")				=> #f)
  (check (string/numbers-ci<>? "1" "")				=> #t)
  (check (string/numbers-ci<>? "" "1")				=> #t)
  (check (string/numbers-ci<>? "1" "1")				=> #f)
  (check (string/numbers-ci<>? "1" "2")				=> #t)
  (check (string/numbers-ci<>? "2" "1")				=> #t)
  (check (string/numbers-ci<>? "a" "ab")			=> #t)
  (check (string/numbers-ci<>? "ab" "a")			=> #t)
  (check (string/numbers-ci<>? "a" "a1")			=> #t)
  (check (string/numbers-ci<>? "a1" "a")			=> #t)
  (check (string/numbers-ci<>? "1" "1a")			=> #t)
  (check (string/numbers-ci<>? "1a" "1")			=> #t)
  (check (string/numbers-ci<>? "A" "a")				=> #f)
  (check (string/numbers-ci<>? "a" "A")				=> #f)

  (check (string/numbers-ci<>? "123" "45")			=> #t)
  (check (string/numbers-ci<>? "45" "123")			=> #t)
  (check (string/numbers-ci<>? "ciao3" "ciao10")		=> #t)
  (check (string/numbers-ci<>? "ciao10" "ciao3")		=> #t)
  (check (string/numbers-ci<>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (string/numbers-ci<>? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (string/numbers-ci<>? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (string/numbers-ci<>? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (string/numbers-ci<>? "foo12" "12foo")			=> #t)
  (check (string/numbers-ci<>? "12foo" "foo12")			=> #t)
  (check (string/numbers-ci<>? "12bar" "foobar")		=> #t)
  (check (string/numbers-ci<>? "12.3" "12.3")			=> #f)
  (check (string/numbers-ci<>? "12.3" "12.10")			=> #t)
  (check (string/numbers-ci<>? "12.10" "12.3")			=> #t)
  (check (string/numbers-ci<>? "12.3" "12,10")			=> #t)
  (check (string/numbers-ci<>? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (string/numbers-ci<? "" "")				=> #f)
  (check (string/numbers-ci<? "a" "")				=> #f)
  (check (string/numbers-ci<? "" "a")				=> #t)
  (check (string/numbers-ci<? "a" "a")				=> #f)
  (check (string/numbers-ci<? "1" "")				=> #f)
  (check (string/numbers-ci<? "" "1")				=> #t)
  (check (string/numbers-ci<? "1" "1")				=> #f)
  (check (string/numbers-ci<? "1" "2")				=> #t)
  (check (string/numbers-ci<? "2" "1")				=> #f)
  (check (string/numbers-ci<? "a" "ab")				=> #t)
  (check (string/numbers-ci<? "ab" "a")				=> #f)
  (check (string/numbers-ci<? "a" "a1")				=> #t)
  (check (string/numbers-ci<? "a1" "a")				=> #f)
  (check (string/numbers-ci<? "1" "1a")				=> #t)
  (check (string/numbers-ci<? "1a" "1")				=> #f)
  (check (string/numbers-ci<? "a" "A")				=> #f)
  (check (string/numbers-ci<? "A" "a")				=> #f)

  (check (string/numbers-ci<? "123" "45")			=> #f)
  (check (string/numbers-ci<? "45" "123")			=> #t)
  (check (string/numbers-ci<? "ciao3" "ciao10")			=> #t)
  (check (string/numbers-ci<? "ciao10" "ciao3")			=> #f)
  (check (string/numbers-ci<? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (string/numbers-ci<? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (string/numbers-ci<? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (string/numbers-ci<? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (string/numbers-ci<? "foo12" "12foo")			=> #f)
  (check (string/numbers-ci<? "12foo" "foo12")			=> #t)
  (check (string/numbers-ci<? "12bar" "foobar")			=> #t)
  (check (string/numbers-ci<? "12.3" "12.3")			=> #f)
  (check (string/numbers-ci<? "12.3" "12.10")			=> #t)
  (check (string/numbers-ci<? "12.10" "12.3")			=> #f)
  (check (string/numbers-ci<? "12.3" "12,10")			=> #f)
  (check (string/numbers-ci<? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (string/numbers-ci<=? "" "")				=> #t)
  (check (string/numbers-ci<=? "a" "")				=> #f)
  (check (string/numbers-ci<=? "" "a")				=> #t)
  (check (string/numbers-ci<=? "a" "a")				=> #t)
  (check (string/numbers-ci<=? "1" "")				=> #f)
  (check (string/numbers-ci<=? "" "1")				=> #t)
  (check (string/numbers-ci<=? "1" "1")				=> #t)
  (check (string/numbers-ci<=? "1" "2")				=> #t)
  (check (string/numbers-ci<=? "2" "1")				=> #f)
  (check (string/numbers-ci<=? "a" "ab")			=> #t)
  (check (string/numbers-ci<=? "ab" "a")			=> #f)
  (check (string/numbers-ci<=? "a" "a1")			=> #t)
  (check (string/numbers-ci<=? "a1" "a")			=> #f)
  (check (string/numbers-ci<=? "1" "1a")			=> #t)
  (check (string/numbers-ci<=? "1a" "1")			=> #f)
  (check (string/numbers-ci<=? "a" "A")				=> #t)
  (check (string/numbers-ci<=? "A" "a")				=> #t)

  (check (string/numbers-ci<=? "123" "45")			=> #f)
  (check (string/numbers-ci<=? "45" "123")			=> #t)
  (check (string/numbers-ci<=? "ciao3" "ciao10")		=> #t)
  (check (string/numbers-ci<=? "ciao10" "ciao3")		=> #f)
  (check (string/numbers-ci<=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (string/numbers-ci<=? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (string/numbers-ci<=? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (string/numbers-ci<=? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (string/numbers-ci<=? "foo12" "12foo")			=> #f)
  (check (string/numbers-ci<=? "12foo" "foo12")			=> #t)
  (check (string/numbers-ci<=? "12bar" "foobar")		=> #t)
  (check (string/numbers-ci<=? "12.3" "12.3")			=> #t)
  (check (string/numbers-ci<=? "12.3" "12.10")			=> #t)
  (check (string/numbers-ci<=? "12.10" "12.3")			=> #f)
  (check (string/numbers-ci<=? "12.3" "12,10")			=> #f)
  (check (string/numbers-ci<=? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (string/numbers-ci>? "" "")				=> #f)
  (check (string/numbers-ci>? "a" "")				=> #t)
  (check (string/numbers-ci>? "" "a")				=> #f)
  (check (string/numbers-ci>? "a" "a")				=> #f)
  (check (string/numbers-ci>? "1" "")				=> #t)
  (check (string/numbers-ci>? "" "1")				=> #f)
  (check (string/numbers-ci>? "1" "1")				=> #f)
  (check (string/numbers-ci>? "1" "2")				=> #f)
  (check (string/numbers-ci>? "2" "1")				=> #t)
  (check (string/numbers-ci>? "a" "ab")				=> #f)
  (check (string/numbers-ci>? "ab" "a")				=> #t)
  (check (string/numbers-ci>? "a" "a1")				=> #f)
  (check (string/numbers-ci>? "a1" "a")				=> #t)
  (check (string/numbers-ci>? "1" "1a")				=> #f)
  (check (string/numbers-ci>? "1a" "1")				=> #t)
  (check (string/numbers-ci>? "a" "A")				=> #f)
  (check (string/numbers-ci>? "A" "a")				=> #f)

  (check (string/numbers-ci>? "123" "45")			=> #t)
  (check (string/numbers-ci>? "45" "123")			=> #f)
  (check (string/numbers-ci>? "ciao3" "ciao10")			=> #f)
  (check (string/numbers-ci>? "ciao10" "ciao3")			=> #t)
  (check (string/numbers-ci>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (string/numbers-ci>? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (string/numbers-ci>? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (string/numbers-ci>? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (string/numbers-ci>? "foo12" "12foo")			=> #t)
  (check (string/numbers-ci>? "12foo" "foo12")			=> #f)
  (check (string/numbers-ci>? "12bar" "foobar")			=> #f)
  (check (string/numbers-ci>? "12.3" "12.3")			=> #f)
  (check (string/numbers-ci>? "12.3" "12.10")			=> #f)
  (check (string/numbers-ci>? "12.10" "12.3")			=> #t)
  (check (string/numbers-ci>? "12.3" "12,10")			=> #t)
  (check (string/numbers-ci>? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check (string/numbers-ci>=? "" "")				=> #t)
  (check (string/numbers-ci>=? "a" "")				=> #t)
  (check (string/numbers-ci>=? "" "a")				=> #f)
  (check (string/numbers-ci>=? "a" "a")				=> #t)
  (check (string/numbers-ci>=? "1" "")				=> #t)
  (check (string/numbers-ci>=? "" "1")				=> #f)
  (check (string/numbers-ci>=? "1" "1")				=> #t)
  (check (string/numbers-ci>=? "1" "2")				=> #f)
  (check (string/numbers-ci>=? "2" "1")				=> #t)
  (check (string/numbers-ci>=? "a" "ab")			=> #f)
  (check (string/numbers-ci>=? "ab" "a")			=> #t)
  (check (string/numbers-ci>=? "a" "a1")			=> #f)
  (check (string/numbers-ci>=? "a1" "a")			=> #t)
  (check (string/numbers-ci>=? "1" "1a")			=> #f)
  (check (string/numbers-ci>=? "1a" "1")			=> #t)
  (check (string/numbers-ci>=? "a" "A")				=> #t)
  (check (string/numbers-ci>=? "A" "a")				=> #t)

  (check (string/numbers-ci>=? "123" "45")			=> #t)
  (check (string/numbers-ci>=? "45" "123")			=> #f)
  (check (string/numbers-ci>=? "ciao3" "ciao10")		=> #f)
  (check (string/numbers-ci>=? "ciao10" "ciao3")		=> #t)
  (check (string/numbers-ci>=? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (string/numbers-ci>=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (string/numbers-ci>=? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (string/numbers-ci>=? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (string/numbers-ci>=? "foo12" "12foo")			=> #t)
  (check (string/numbers-ci>=? "12foo" "foo12")			=> #f)
  (check (string/numbers-ci>=? "12bar" "foobar")		=> #f)
  (check (string/numbers-ci>=? "12.3" "12.3")			=> #t)
  (check (string/numbers-ci>=? "12.3" "12.10")			=> #f)
  (check (string/numbers-ci>=? "12.10" "12.3")			=> #t)
  (check (string/numbers-ci>=? "12.3" "12,10")			=> #t)
  (check (string/numbers-ci>=? "12,10" "12.3")			=> #f)

  #t)


(parameterise ((check-test-name 'comparison-dictionary-string/number-case-sensitive))

  (check (string/numbers-dictionary=? "" "")				=> #t)
  (check (string/numbers-dictionary=? "a" "")				=> #f)
  (check (string/numbers-dictionary=? "" "a")				=> #f)
  (check (string/numbers-dictionary=? "a" "a")				=> #t)
  (check (string/numbers-dictionary=? "1" "")				=> #f)
  (check (string/numbers-dictionary=? "" "1")				=> #f)
  (check (string/numbers-dictionary=? "1" "1")				=> #t)
  (check (string/numbers-dictionary=? "1" "2")				=> #f)
  (check (string/numbers-dictionary=? "2" "1")				=> #f)
  (check (string/numbers-dictionary=? "a" "ab")				=> #f)
  (check (string/numbers-dictionary=? "ab" "a")				=> #f)
  (check (string/numbers-dictionary=? "a" "a1")				=> #f)
  (check (string/numbers-dictionary=? "a1" "a")				=> #f)
  (check (string/numbers-dictionary=? "1" "1a")				=> #f)
  (check (string/numbers-dictionary=? "1a" "1")				=> #f)

  (check (string/numbers-dictionary=? "123" "45")			=> #f)
  (check (string/numbers-dictionary=? "45" "123")			=> #f)
  (check (string/numbers-dictionary=? "ciao3" "ciao10")			=> #f)
  (check (string/numbers-dictionary=? "ciao10" "ciao3")			=> #f)
  (check (string/numbers-dictionary=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (string/numbers-dictionary=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (string/numbers-dictionary=? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (string/numbers-dictionary=? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (string/numbers-dictionary=? "foo12" "12foo")			=> #f)
  (check (string/numbers-dictionary=? "12foo" "foo12")			=> #f)
  (check (string/numbers-dictionary=? "12bar" "foobar")			=> #f)
  (check (string/numbers-dictionary=? "12.3" "12.3")			=> #t)
  (check (string/numbers-dictionary=? "12.3" "12.10")			=> #f)
  (check (string/numbers-dictionary=? "12.10" "12.3")			=> #f)
  (check (string/numbers-dictionary=? "12.3" "12,10")			=> #f)
  (check (string/numbers-dictionary=? "12,10" "12.3")			=> #f)

  (check (string/numbers-dictionary=? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #f)
  (check (string/numbers-dictionary=? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #f)

;;; --------------------------------------------------------------------

  (check (string/numbers-dictionary<>? "" "")				=> #f)
  (check (string/numbers-dictionary<>? "a" "")				=> #t)
  (check (string/numbers-dictionary<>? "" "a")				=> #t)
  (check (string/numbers-dictionary<>? "a" "a")				=> #f)
  (check (string/numbers-dictionary<>? "1" "")				=> #t)
  (check (string/numbers-dictionary<>? "" "1")				=> #t)
  (check (string/numbers-dictionary<>? "1" "1")				=> #f)
  (check (string/numbers-dictionary<>? "1" "2")				=> #t)
  (check (string/numbers-dictionary<>? "2" "1")				=> #t)
  (check (string/numbers-dictionary<>? "a" "ab")			=> #t)
  (check (string/numbers-dictionary<>? "ab" "a")			=> #t)
  (check (string/numbers-dictionary<>? "a" "a1")			=> #t)
  (check (string/numbers-dictionary<>? "a1" "a")			=> #t)
  (check (string/numbers-dictionary<>? "1" "1a")			=> #t)
  (check (string/numbers-dictionary<>? "1a" "1")			=> #t)

  (check (string/numbers-dictionary<>? "123" "45")			=> #t)
  (check (string/numbers-dictionary<>? "45" "123")			=> #t)
  (check (string/numbers-dictionary<>? "ciao3" "ciao10")		=> #t)
  (check (string/numbers-dictionary<>? "ciao10" "ciao3")		=> #t)
  (check (string/numbers-dictionary<>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (string/numbers-dictionary<>? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (string/numbers-dictionary<>? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (string/numbers-dictionary<>? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (string/numbers-dictionary<>? "foo12" "12foo")			=> #t)
  (check (string/numbers-dictionary<>? "12foo" "foo12")			=> #t)
  (check (string/numbers-dictionary<>? "12bar" "foobar")		=> #t)
  (check (string/numbers-dictionary<>? "12.3" "12.3")			=> #f)
  (check (string/numbers-dictionary<>? "12.3" "12.10")			=> #t)
  (check (string/numbers-dictionary<>? "12.10" "12.3")			=> #t)
  (check (string/numbers-dictionary<>? "12.3" "12,10")			=> #t)
  (check (string/numbers-dictionary<>? "12,10" "12.3")			=> #t)

  (check (string/numbers-dictionary<>? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #t)
  (check (string/numbers-dictionary<>? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #t)

;;; --------------------------------------------------------------------

  (check (string/numbers-dictionary<? "" "")				=> #f)
  (check (string/numbers-dictionary<? "a" "")				=> #f)
  (check (string/numbers-dictionary<? "" "a")				=> #t)
  (check (string/numbers-dictionary<? "a" "a")				=> #f)
  (check (string/numbers-dictionary<? "1" "")				=> #f)
  (check (string/numbers-dictionary<? "" "1")				=> #t)
  (check (string/numbers-dictionary<? "1" "1")				=> #f)
  (check (string/numbers-dictionary<? "1" "2")				=> #t)
  (check (string/numbers-dictionary<? "2" "1")				=> #f)
  (check (string/numbers-dictionary<? "a" "ab")				=> #t)
  (check (string/numbers-dictionary<? "ab" "a")				=> #f)
  (check (string/numbers-dictionary<? "a" "a1")				=> #t)
  (check (string/numbers-dictionary<? "a1" "a")				=> #f)
  (check (string/numbers-dictionary<? "1" "1a")				=> #t)
  (check (string/numbers-dictionary<? "1a" "1")				=> #f)

  (check (string/numbers-dictionary<? "123" "45")			=> #f)
  (check (string/numbers-dictionary<? "45" "123")			=> #t)
  (check (string/numbers-dictionary<? "ciao3" "ciao10")			=> #t)
  (check (string/numbers-dictionary<? "ciao10" "ciao3")			=> #f)
  (check (string/numbers-dictionary<? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (string/numbers-dictionary<? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (string/numbers-dictionary<? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (string/numbers-dictionary<? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (string/numbers-dictionary<? "foo12" "12foo")			=> #f)
  (check (string/numbers-dictionary<? "12foo" "foo12")			=> #t)
  (check (string/numbers-dictionary<? "12bar" "foobar")			=> #t)
  (check (string/numbers-dictionary<? "12.3" "12.3")			=> #f)
  (check (string/numbers-dictionary<? "12.3" "12.10")			=> #t)
  (check (string/numbers-dictionary<? "12.10" "12.3")			=> #f)
  (check (string/numbers-dictionary<? "12.3" "12,10")			=> #f)
  (check (string/numbers-dictionary<? "12,10" "12.3")			=> #t)

  (check 'this (string/numbers-dictionary<? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #f)
  (check (string/numbers-dictionary<? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #t)

;;; --------------------------------------------------------------------

  (check (string/numbers-dictionary<=? "" "")				=> #t)
  (check (string/numbers-dictionary<=? "a" "")				=> #f)
  (check (string/numbers-dictionary<=? "" "a")				=> #t)
  (check (string/numbers-dictionary<=? "a" "a")				=> #t)
  (check (string/numbers-dictionary<=? "1" "")				=> #f)
  (check (string/numbers-dictionary<=? "" "1")				=> #t)
  (check (string/numbers-dictionary<=? "1" "1")				=> #t)
  (check (string/numbers-dictionary<=? "1" "2")				=> #t)
  (check (string/numbers-dictionary<=? "2" "1")				=> #f)
  (check (string/numbers-dictionary<=? "a" "ab")			=> #t)
  (check (string/numbers-dictionary<=? "ab" "a")			=> #f)
  (check (string/numbers-dictionary<=? "a" "a1")			=> #t)
  (check (string/numbers-dictionary<=? "a1" "a")			=> #f)
  (check (string/numbers-dictionary<=? "1" "1a")			=> #t)
  (check (string/numbers-dictionary<=? "1a" "1")			=> #f)

  (check (string/numbers-dictionary<=? "123" "45")			=> #f)
  (check (string/numbers-dictionary<=? "45" "123")			=> #t)
  (check (string/numbers-dictionary<=? "ciao3" "ciao10")		=> #t)
  (check (string/numbers-dictionary<=? "ciao10" "ciao3")		=> #f)
  (check (string/numbers-dictionary<=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (string/numbers-dictionary<=? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (string/numbers-dictionary<=? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (string/numbers-dictionary<=? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (string/numbers-dictionary<=? "foo12" "12foo")			=> #f)
  (check (string/numbers-dictionary<=? "12foo" "foo12")			=> #t)
  (check (string/numbers-dictionary<=? "12bar" "foobar")		=> #t)
  (check (string/numbers-dictionary<=? "12.3" "12.3")			=> #t)
  (check (string/numbers-dictionary<=? "12.3" "12.10")			=> #t)
  (check (string/numbers-dictionary<=? "12.10" "12.3")			=> #f)
  (check (string/numbers-dictionary<=? "12.3" "12,10")			=> #f)
  (check (string/numbers-dictionary<=? "12,10" "12.3")			=> #t)

  (check (string/numbers-dictionary<=? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #f)
  (check (string/numbers-dictionary<=? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #t)

;;; --------------------------------------------------------------------

  (check (string/numbers-dictionary>? "" "")				=> #f)
  (check (string/numbers-dictionary>? "a" "")				=> #t)
  (check (string/numbers-dictionary>? "" "a")				=> #f)
  (check (string/numbers-dictionary>? "a" "a")				=> #f)
  (check (string/numbers-dictionary>? "1" "")				=> #t)
  (check (string/numbers-dictionary>? "" "1")				=> #f)
  (check (string/numbers-dictionary>? "1" "1")				=> #f)
  (check (string/numbers-dictionary>? "1" "2")				=> #f)
  (check (string/numbers-dictionary>? "2" "1")				=> #t)
  (check (string/numbers-dictionary>? "a" "ab")				=> #f)
  (check (string/numbers-dictionary>? "ab" "a")				=> #t)
  (check (string/numbers-dictionary>? "a" "a1")				=> #f)
  (check (string/numbers-dictionary>? "a1" "a")				=> #t)
  (check (string/numbers-dictionary>? "1" "1a")				=> #f)
  (check (string/numbers-dictionary>? "1a" "1")				=> #t)

  (check (string/numbers-dictionary>? "123" "45")			=> #t)
  (check (string/numbers-dictionary>? "45" "123")			=> #f)
  (check (string/numbers-dictionary>? "ciao3" "ciao10")			=> #f)
  (check (string/numbers-dictionary>? "ciao10" "ciao3")			=> #t)
  (check (string/numbers-dictionary>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (string/numbers-dictionary>? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (string/numbers-dictionary>? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (string/numbers-dictionary>? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (string/numbers-dictionary>? "foo12" "12foo")			=> #t)
  (check (string/numbers-dictionary>? "12foo" "foo12")			=> #f)
  (check (string/numbers-dictionary>? "12bar" "foobar")			=> #f)
  (check (string/numbers-dictionary>? "12.3" "12.3")			=> #f)
  (check (string/numbers-dictionary>? "12.3" "12.10")			=> #f)
  (check (string/numbers-dictionary>? "12.10" "12.3")			=> #t)
  (check (string/numbers-dictionary>? "12.3" "12,10")			=> #t)
  (check (string/numbers-dictionary>? "12,10" "12.3")			=> #f)

  (check (string/numbers-dictionary>? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #t)
  (check (string/numbers-dictionary>? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #f)

;;; --------------------------------------------------------------------

  (check (string/numbers-dictionary>=? "" "")				=> #t)
  (check (string/numbers-dictionary>=? "a" "")				=> #t)
  (check (string/numbers-dictionary>=? "" "a")				=> #f)
  (check (string/numbers-dictionary>=? "a" "a")				=> #t)
  (check (string/numbers-dictionary>=? "1" "")				=> #t)
  (check (string/numbers-dictionary>=? "" "1")				=> #f)
  (check (string/numbers-dictionary>=? "1" "1")				=> #t)
  (check (string/numbers-dictionary>=? "1" "2")				=> #f)
  (check (string/numbers-dictionary>=? "2" "1")				=> #t)
  (check (string/numbers-dictionary>=? "a" "ab")			=> #f)
  (check (string/numbers-dictionary>=? "ab" "a")			=> #t)
  (check (string/numbers-dictionary>=? "a" "a1")			=> #f)
  (check (string/numbers-dictionary>=? "a1" "a")			=> #t)
  (check (string/numbers-dictionary>=? "1" "1a")			=> #f)
  (check (string/numbers-dictionary>=? "1a" "1")			=> #t)

  (check (string/numbers-dictionary>=? "123" "45")			=> #t)
  (check (string/numbers-dictionary>=? "45" "123")			=> #f)
  (check (string/numbers-dictionary>=? "ciao3" "ciao10")		=> #f)
  (check (string/numbers-dictionary>=? "ciao10" "ciao3")		=> #t)
  (check (string/numbers-dictionary>=? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (string/numbers-dictionary>=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (string/numbers-dictionary>=? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (string/numbers-dictionary>=? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (string/numbers-dictionary>=? "foo12" "12foo")			=> #t)
  (check (string/numbers-dictionary>=? "12foo" "foo12")			=> #f)
  (check (string/numbers-dictionary>=? "12bar" "foobar")		=> #f)
  (check (string/numbers-dictionary>=? "12.3" "12.3")			=> #t)
  (check (string/numbers-dictionary>=? "12.3" "12.10")			=> #f)
  (check (string/numbers-dictionary>=? "12.10" "12.3")			=> #t)
  (check (string/numbers-dictionary>=? "12.3" "12,10")			=> #t)
  (check (string/numbers-dictionary>=? "12,10" "12.3")			=> #f)

  (check (string/numbers-dictionary>=? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #t)
  (check (string/numbers-dictionary>=? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #f)

  #t)


(parameterise ((check-test-name 'comparison-dictionary-string/number-case-insensitive))

  (check (string/numbers-dictionary-compare-ci "" "")				=> 0)
  (check (string/numbers-dictionary-compare-ci "a" "")				=> +1)
  (check (string/numbers-dictionary-compare-ci "" "a")				=> -1)
  (check (string/numbers-dictionary-compare-ci "a" "a")				=> 0)
  (check (string/numbers-dictionary-compare-ci "1" "")				=> +1)
  (check (string/numbers-dictionary-compare-ci "" "1")				=> -1)
  (check (string/numbers-dictionary-compare-ci "1" "1")				=> 0)
  (check (string/numbers-dictionary-compare-ci "1" "2")				=> -1)
  (check (string/numbers-dictionary-compare-ci "2" "1")				=> +1)
  (check (string/numbers-dictionary-compare-ci "a" "ab")			=> -1)
  (check (string/numbers-dictionary-compare-ci "ab" "a")			=> +1)
  (check (string/numbers-dictionary-compare-ci "a" "a1")			=> -1)
  (check (string/numbers-dictionary-compare-ci "a1" "a")			=> +1)
  (check (string/numbers-dictionary-compare-ci "1" "1a")			=> -1)
  (check (string/numbers-dictionary-compare-ci "1a" "1")			=> +1)
  (check (string/numbers-dictionary-compare-ci "a" "A")				=> 0)
  (check (string/numbers-dictionary-compare-ci "A" "a")				=> 0)

  (check (string/numbers-dictionary-compare-ci "123" "45")			=> +1)
  (check (string/numbers-dictionary-compare-ci "45" "123")			=> -1)
  (check (string/numbers-dictionary-compare-ci "ciao3" "ciao10")		=> -1)
  (check (string/numbers-dictionary-compare-ci "ciao10" "ciao3")		=> +1)
  (check (string/numbers-dictionary-compare-ci "foo4bar3zab10" "foo4bar3zab2")	=> +1)
  (check (string/numbers-dictionary-compare-ci "foo4bar3zab2" "foo4bar3zab10")	=> -1)
  (check (string/numbers-dictionary-compare-ci "foo4bar3zab" "foo4bar10")	=> -1)
  (check (string/numbers-dictionary-compare-ci "foo4bar10" "foo4bar3zab")	=> +1)
  (check (string/numbers-dictionary-compare-ci "foo12" "12foo")			=> +1)
  (check (string/numbers-dictionary-compare-ci "12foo" "foo12")			=> -1)
  (check (string/numbers-dictionary-compare-ci "12bar" "foobar")		=> -1)
  (check (string/numbers-dictionary-compare-ci "12.3" "12.3")			=> 0)
  (check (string/numbers-dictionary-compare-ci "12.3" "12.10")			=> -1)
  (check (string/numbers-dictionary-compare-ci "12.10" "12.3")			=> +1)
  (check (string/numbers-dictionary-compare-ci "12.3" "12,10")			=> +1)
  (check (string/numbers-dictionary-compare-ci "12,10" "12.3")			=> -1)

;;; --------------------------------------------------------------------

  (check (string/numbers-dictionary-ci=? "" "")				=> #t)
  (check (string/numbers-dictionary-ci=? "a" "")			=> #f)
  (check (string/numbers-dictionary-ci=? "" "a")			=> #f)
  (check (string/numbers-dictionary-ci=? "a" "a")			=> #t)
  (check (string/numbers-dictionary-ci=? "1" "")			=> #f)
  (check (string/numbers-dictionary-ci=? "" "1")			=> #f)
  (check (string/numbers-dictionary-ci=? "1" "1")			=> #t)
  (check (string/numbers-dictionary-ci=? "1" "2")			=> #f)
  (check (string/numbers-dictionary-ci=? "2" "1")			=> #f)
  (check (string/numbers-dictionary-ci=? "a" "ab")			=> #f)
  (check (string/numbers-dictionary-ci=? "ab" "a")			=> #f)
  (check (string/numbers-dictionary-ci=? "a" "a1")			=> #f)
  (check (string/numbers-dictionary-ci=? "a1" "a")			=> #f)
  (check (string/numbers-dictionary-ci=? "1" "1a")			=> #f)
  (check (string/numbers-dictionary-ci=? "1a" "1")			=> #f)
  (check (string/numbers-dictionary-ci=? "a" "A")			=> #t)
  (check (string/numbers-dictionary-ci=? "A" "a")			=> #t)

  (check (string/numbers-dictionary-ci=? "123" "45")			=> #f)
  (check (string/numbers-dictionary-ci=? "45" "123")			=> #f)
  (check (string/numbers-dictionary-ci=? "ciao3" "ciao10")		=> #f)
  (check (string/numbers-dictionary-ci=? "ciao10" "ciao3")		=> #f)
  (check (string/numbers-dictionary-ci=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (string/numbers-dictionary-ci=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (string/numbers-dictionary-ci=? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (string/numbers-dictionary-ci=? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (string/numbers-dictionary-ci=? "foo12" "12foo")		=> #f)
  (check (string/numbers-dictionary-ci=? "12foo" "foo12")		=> #f)
  (check (string/numbers-dictionary-ci=? "12bar" "foobar")		=> #f)
  (check (string/numbers-dictionary-ci=? "12.3" "12.3")			=> #t)
  (check (string/numbers-dictionary-ci=? "12.3" "12.10")		=> #f)
  (check (string/numbers-dictionary-ci=? "12.10" "12.3")		=> #f)
  (check (string/numbers-dictionary-ci=? "12.3" "12,10")		=> #f)
  (check (string/numbers-dictionary-ci=? "12,10" "12.3")		=> #f)

;;; --------------------------------------------------------------------

  (check (string/numbers-dictionary-ci<>? "" "")			=> #f)
  (check (string/numbers-dictionary-ci<>? "a" "")			=> #t)
  (check (string/numbers-dictionary-ci<>? "" "a")			=> #t)
  (check (string/numbers-dictionary-ci<>? "a" "a")			=> #f)
  (check (string/numbers-dictionary-ci<>? "1" "")			=> #t)
  (check (string/numbers-dictionary-ci<>? "" "1")			=> #t)
  (check (string/numbers-dictionary-ci<>? "1" "1")			=> #f)
  (check (string/numbers-dictionary-ci<>? "1" "2")			=> #t)
  (check (string/numbers-dictionary-ci<>? "2" "1")			=> #t)
  (check (string/numbers-dictionary-ci<>? "a" "ab")			=> #t)
  (check (string/numbers-dictionary-ci<>? "ab" "a")			=> #t)
  (check (string/numbers-dictionary-ci<>? "a" "a1")			=> #t)
  (check (string/numbers-dictionary-ci<>? "a1" "a")			=> #t)
  (check (string/numbers-dictionary-ci<>? "1" "1a")			=> #t)
  (check (string/numbers-dictionary-ci<>? "1a" "1")			=> #t)
  (check (string/numbers-dictionary-ci<>? "A" "a")			=> #f)
  (check (string/numbers-dictionary-ci<>? "a" "A")			=> #f)

  (check (string/numbers-dictionary-ci<>? "123" "45")			=> #t)
  (check (string/numbers-dictionary-ci<>? "45" "123")			=> #t)
  (check (string/numbers-dictionary-ci<>? "ciao3" "ciao10")		=> #t)
  (check (string/numbers-dictionary-ci<>? "ciao10" "ciao3")		=> #t)
  (check (string/numbers-dictionary-ci<>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (string/numbers-dictionary-ci<>? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (string/numbers-dictionary-ci<>? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (string/numbers-dictionary-ci<>? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (string/numbers-dictionary-ci<>? "foo12" "12foo")		=> #t)
  (check (string/numbers-dictionary-ci<>? "12foo" "foo12")		=> #t)
  (check (string/numbers-dictionary-ci<>? "12bar" "foobar")		=> #t)
  (check (string/numbers-dictionary-ci<>? "12.3" "12.3")		=> #f)
  (check (string/numbers-dictionary-ci<>? "12.3" "12.10")		=> #t)
  (check (string/numbers-dictionary-ci<>? "12.10" "12.3")		=> #t)
  (check (string/numbers-dictionary-ci<>? "12.3" "12,10")		=> #t)
  (check (string/numbers-dictionary-ci<>? "12,10" "12.3")		=> #t)

;;; --------------------------------------------------------------------

  (check (string/numbers-dictionary-ci<? "" "")				=> #f)
  (check (string/numbers-dictionary-ci<? "a" "")			=> #f)
  (check (string/numbers-dictionary-ci<? "" "a")			=> #t)
  (check (string/numbers-dictionary-ci<? "a" "a")			=> #f)
  (check (string/numbers-dictionary-ci<? "1" "")			=> #f)
  (check (string/numbers-dictionary-ci<? "" "1")			=> #t)
  (check (string/numbers-dictionary-ci<? "1" "1")			=> #f)
  (check (string/numbers-dictionary-ci<? "1" "2")			=> #t)
  (check (string/numbers-dictionary-ci<? "2" "1")			=> #f)
  (check (string/numbers-dictionary-ci<? "a" "ab")			=> #t)
  (check (string/numbers-dictionary-ci<? "ab" "a")			=> #f)
  (check (string/numbers-dictionary-ci<? "a" "a1")			=> #t)
  (check (string/numbers-dictionary-ci<? "a1" "a")			=> #f)
  (check (string/numbers-dictionary-ci<? "1" "1a")			=> #t)
  (check (string/numbers-dictionary-ci<? "1a" "1")			=> #f)
  (check (string/numbers-dictionary-ci<? "a" "A")			=> #f)
  (check (string/numbers-dictionary-ci<? "A" "a")			=> #f)

  (check (string/numbers-dictionary-ci<? "123" "45")			=> #f)
  (check (string/numbers-dictionary-ci<? "45" "123")			=> #t)
  (check (string/numbers-dictionary-ci<? "ciao3" "ciao10")		=> #t)
  (check (string/numbers-dictionary-ci<? "ciao10" "ciao3")		=> #f)
  (check (string/numbers-dictionary-ci<? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (string/numbers-dictionary-ci<? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (string/numbers-dictionary-ci<? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (string/numbers-dictionary-ci<? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (string/numbers-dictionary-ci<? "foo12" "12foo")		=> #f)
  (check (string/numbers-dictionary-ci<? "12foo" "foo12")		=> #t)
  (check (string/numbers-dictionary-ci<? "12bar" "foobar")		=> #t)
  (check (string/numbers-dictionary-ci<? "12.3" "12.3")			=> #f)
  (check (string/numbers-dictionary-ci<? "12.3" "12.10")		=> #t)
  (check (string/numbers-dictionary-ci<? "12.10" "12.3")		=> #f)
  (check (string/numbers-dictionary-ci<? "12.3" "12,10")		=> #f)
  (check (string/numbers-dictionary-ci<? "12,10" "12.3")		=> #t)

;;; --------------------------------------------------------------------

  (check (string/numbers-dictionary-ci<=? "" "")			=> #t)
  (check (string/numbers-dictionary-ci<=? "a" "")			=> #f)
  (check (string/numbers-dictionary-ci<=? "" "a")			=> #t)
  (check (string/numbers-dictionary-ci<=? "a" "a")			=> #t)
  (check (string/numbers-dictionary-ci<=? "1" "")			=> #f)
  (check (string/numbers-dictionary-ci<=? "" "1")			=> #t)
  (check (string/numbers-dictionary-ci<=? "1" "1")			=> #t)
  (check (string/numbers-dictionary-ci<=? "1" "2")			=> #t)
  (check (string/numbers-dictionary-ci<=? "2" "1")			=> #f)
  (check (string/numbers-dictionary-ci<=? "a" "ab")			=> #t)
  (check (string/numbers-dictionary-ci<=? "ab" "a")			=> #f)
  (check (string/numbers-dictionary-ci<=? "a" "a1")			=> #t)
  (check (string/numbers-dictionary-ci<=? "a1" "a")			=> #f)
  (check (string/numbers-dictionary-ci<=? "1" "1a")			=> #t)
  (check (string/numbers-dictionary-ci<=? "1a" "1")			=> #f)
  (check (string/numbers-dictionary-ci<=? "a" "A")			=> #t)
  (check (string/numbers-dictionary-ci<=? "A" "a")			=> #t)

  (check (string/numbers-dictionary-ci<=? "123" "45")			=> #f)
  (check (string/numbers-dictionary-ci<=? "45" "123")			=> #t)
  (check (string/numbers-dictionary-ci<=? "ciao3" "ciao10")		=> #t)
  (check (string/numbers-dictionary-ci<=? "ciao10" "ciao3")		=> #f)
  (check (string/numbers-dictionary-ci<=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (string/numbers-dictionary-ci<=? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (string/numbers-dictionary-ci<=? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (string/numbers-dictionary-ci<=? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (string/numbers-dictionary-ci<=? "foo12" "12foo")		=> #f)
  (check (string/numbers-dictionary-ci<=? "12foo" "foo12")		=> #t)
  (check (string/numbers-dictionary-ci<=? "12bar" "foobar")		=> #t)
  (check (string/numbers-dictionary-ci<=? "12.3" "12.3")		=> #t)
  (check (string/numbers-dictionary-ci<=? "12.3" "12.10")		=> #t)
  (check (string/numbers-dictionary-ci<=? "12.10" "12.3")		=> #f)
  (check (string/numbers-dictionary-ci<=? "12.3" "12,10")		=> #f)
  (check (string/numbers-dictionary-ci<=? "12,10" "12.3")		=> #t)

;;; --------------------------------------------------------------------

  (check (string/numbers-dictionary-ci>? "" "")				=> #f)
  (check (string/numbers-dictionary-ci>? "a" "")			=> #t)
  (check (string/numbers-dictionary-ci>? "" "a")			=> #f)
  (check (string/numbers-dictionary-ci>? "a" "a")			=> #f)
  (check (string/numbers-dictionary-ci>? "1" "")			=> #t)
  (check (string/numbers-dictionary-ci>? "" "1")			=> #f)
  (check (string/numbers-dictionary-ci>? "1" "1")			=> #f)
  (check (string/numbers-dictionary-ci>? "1" "2")			=> #f)
  (check (string/numbers-dictionary-ci>? "2" "1")			=> #t)
  (check (string/numbers-dictionary-ci>? "a" "ab")			=> #f)
  (check (string/numbers-dictionary-ci>? "ab" "a")			=> #t)
  (check (string/numbers-dictionary-ci>? "a" "a1")			=> #f)
  (check (string/numbers-dictionary-ci>? "a1" "a")			=> #t)
  (check (string/numbers-dictionary-ci>? "1" "1a")			=> #f)
  (check (string/numbers-dictionary-ci>? "1a" "1")			=> #t)
  (check (string/numbers-dictionary-ci>? "a" "A")			=> #f)
  (check (string/numbers-dictionary-ci>? "A" "a")			=> #f)

  (check (string/numbers-dictionary-ci>? "123" "45")			=> #t)
  (check (string/numbers-dictionary-ci>? "45" "123")			=> #f)
  (check (string/numbers-dictionary-ci>? "ciao3" "ciao10")		=> #f)
  (check (string/numbers-dictionary-ci>? "ciao10" "ciao3")		=> #t)
  (check (string/numbers-dictionary-ci>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (string/numbers-dictionary-ci>? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (string/numbers-dictionary-ci>? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (string/numbers-dictionary-ci>? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (string/numbers-dictionary-ci>? "foo12" "12foo")		=> #t)
  (check (string/numbers-dictionary-ci>? "12foo" "foo12")		=> #f)
  (check (string/numbers-dictionary-ci>? "12bar" "foobar")		=> #f)
  (check (string/numbers-dictionary-ci>? "12.3" "12.3")			=> #f)
  (check (string/numbers-dictionary-ci>? "12.3" "12.10")		=> #f)
  (check (string/numbers-dictionary-ci>? "12.10" "12.3")		=> #t)
  (check (string/numbers-dictionary-ci>? "12.3" "12,10")		=> #t)
  (check (string/numbers-dictionary-ci>? "12,10" "12.3")		=> #f)

;;; --------------------------------------------------------------------

  (check (string/numbers-dictionary-ci>=? "" "")			=> #t)
  (check (string/numbers-dictionary-ci>=? "a" "")			=> #t)
  (check (string/numbers-dictionary-ci>=? "" "a")			=> #f)
  (check (string/numbers-dictionary-ci>=? "a" "a")			=> #t)
  (check (string/numbers-dictionary-ci>=? "1" "")			=> #t)
  (check (string/numbers-dictionary-ci>=? "" "1")			=> #f)
  (check (string/numbers-dictionary-ci>=? "1" "1")			=> #t)
  (check (string/numbers-dictionary-ci>=? "1" "2")			=> #f)
  (check (string/numbers-dictionary-ci>=? "2" "1")			=> #t)
  (check (string/numbers-dictionary-ci>=? "a" "ab")			=> #f)
  (check (string/numbers-dictionary-ci>=? "ab" "a")			=> #t)
  (check (string/numbers-dictionary-ci>=? "a" "a1")			=> #f)
  (check (string/numbers-dictionary-ci>=? "a1" "a")			=> #t)
  (check (string/numbers-dictionary-ci>=? "1" "1a")			=> #f)
  (check (string/numbers-dictionary-ci>=? "1a" "1")			=> #t)
  (check (string/numbers-dictionary-ci>=? "a" "A")			=> #t)
  (check (string/numbers-dictionary-ci>=? "A" "a")			=> #t)

  (check (string/numbers-dictionary-ci>=? "123" "45")			=> #t)
  (check (string/numbers-dictionary-ci>=? "45" "123")			=> #f)
  (check (string/numbers-dictionary-ci>=? "ciao3" "ciao10")		=> #f)
  (check (string/numbers-dictionary-ci>=? "ciao10" "ciao3")		=> #t)
  (check (string/numbers-dictionary-ci>=? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (string/numbers-dictionary-ci>=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (string/numbers-dictionary-ci>=? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (string/numbers-dictionary-ci>=? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (string/numbers-dictionary-ci>=? "foo12" "12foo")		=> #t)
  (check (string/numbers-dictionary-ci>=? "12foo" "foo12")		=> #f)
  (check (string/numbers-dictionary-ci>=? "12bar" "foobar")		=> #f)
  (check (string/numbers-dictionary-ci>=? "12.3" "12.3")		=> #t)
  (check (string/numbers-dictionary-ci>=? "12.3" "12.10")		=> #f)
  (check (string/numbers-dictionary-ci>=? "12.10" "12.3")		=> #t)
  (check (string/numbers-dictionary-ci>=? "12.3" "12,10")		=> #t)
  (check (string/numbers-dictionary-ci>=? "12,10" "12.3")		=> #f)

  #t)


(parameterise ((check-test-name 'mapping))

  (check
      (let ((str (string-copy "abcd")))
	(string-map! (lambda (i ch) (char-upcase ch))
		     str)
	str)
    => "ABCD")

  (check
      (let ((str (string-copy "abcd")))
	(string-map! (lambda (i ch-a ch-b) (if (even? i) ch-a ch-b))
		     str "0123")
	str)
    => "a1c3")

  (check
      (let ((str (string-copy "")))
	(string-map! (lambda (i ch) (char-upcase ch))
		     str)
	str)
    => "")

;;; --------------------------------------------------------------------

  (check
      (let ((str (string-copy "abcd")))
	(string-map*! (lambda (i ch) (char-upcase ch))
		      str)
	str)
    => "ABCD")

  (check
      (let ((str (string-copy "abcd")))
	(string-map*! (lambda (i ch-a ch-b) (if (even? i) ch-a ch-b))
		      str "01234")
	str)
    => "a1c3")

  (check
      (let ((str (string-copy "")))
	(string-map*! (lambda (i ch) (char-upcase ch))
		      str)
	str)
    => "")

;;; --------------------------------------------------------------------

  (check
      (cadr (with-result
	     (string-for-each* (lambda (i ch) (add-result (list i ch)))
			       "abcd")))
    => '((0 #\a)
	 (1 #\b)
	 (2 #\c)
	 (3 #\d)))

  (check
      (cadr (with-result
	     (string-for-each* (lambda (i ch-a ch-b) (add-result (list i ch-a ch-b)))
			       "abcd" "01234")))
    => '((0 #\a #\0)
	 (1 #\b #\1)
	 (2 #\c #\2)
	 (3 #\d #\3)))

  (check
      (cadr (with-result
	     (string-for-each* (lambda (i ch) (add-result (list i ch)))
			       "")))
    => '())

;;; --------------------------------------------------------------------

  (check
      (substring-map (lambda (ch) (char-upcase ch))
		     "abcd")
    => "ABCD")


  (check
      (substring-map (lambda (ch) (char-upcase ch))
		     (view "abcd" (start 1) (past 3)))
    => "BC")

  (check
      (substring-map (lambda (ch) (char-upcase ch))
		     "")
    => "")

;;; --------------------------------------------------------------------

  (check
      (let ((str (string-copy "abcd")))
	(substring-map! (lambda (ch) (char-upcase ch))
			str)
	str)
    => "ABCD")

  (check
      (let ((str (string-copy "abcd")))
	(substring-map! (lambda (ch) (char-upcase ch))
			(view str (start 1) (past 3)))
	str)
    => "aBCd")

  (check
      (let ((str ""))
	(substring-map! (lambda (ch) (char-upcase ch))
			str)
	str)
    => "")

;;; --------------------------------------------------------------------

  (check
      (cadr (with-result
	     (substring-for-each add-result
				 "abcd")))
    => '(#\a #\b #\c #\d))

  (check
      (cadr (with-result
	     (substring-for-each add-result
				 (view "abcd" (start 1) (past 3)))))
    => '(#\b #\c))

  (check
      (cadr (with-result
	     (substring-for-each add-result "")))
    => '())


  )


(parameterise ((check-test-name 'case))

  (check
      (string-upcase* "abcd")
    => "ABCD")

  (check
      (string-upcase* "123abcd")
    => "123ABCD")

  (check
      (string-upcase* "---abcd")
    => "---ABCD")

  (check
      (string-upcase* "abcd efgh")
    => "ABCD EFGH")

;;; --------------------------------------------------------------------

  (check
      (let* ((str (string-copy "abcd")))
	(string-upcase*! str)
	str)
    => "ABCD")

  (check
      (let* ((str (string-copy "123abcd")))
	(string-upcase*! str)
	str)
    => "123ABCD")

  (check
      (let* ((str (string-copy "---abcd")))
	(string-upcase*! str)
	str)
    => "---ABCD")

  (check
      (let* ((str (string-copy "abcd efgh")))
	(string-upcase*! str)
	str)
    => "ABCD EFGH")

;;; --------------------------------------------------------------------

  (check
      (string-downcase* "ABCD")
    => "abcd")

  (check
      (string-downcase* "123AbcD")
    => "123abcd")

  (check
      (string-downcase* "---aBCd")
    => "---abcd")

  (check
      (string-downcase* "abcd EFGH")
    => "abcd efgh")

;;; --------------------------------------------------------------------

  (check
      (let* ((str (string-copy "aBCd")))
	(string-downcase*! str)
	str)
    => "abcd")

  (check
      (let* ((str (string-copy "123ABcd")))
	(string-downcase*! str)
	str)
    => "123abcd")

  (check
      (let* ((str (string-copy "---aBCD")))
	(string-downcase*! str)
	str)
    => "---abcd")

  (check
      (let* ((str (string-copy "abCD Efgh")))
	(string-downcase*! str)
	str)
    => "abcd efgh")

;;; --------------------------------------------------------------------

  (check
      (string-titlecase* "abcd")
    => "Abcd")

  (check
      (string-titlecase* "123abcd")
    => "123Abcd")

  (check
      (string-titlecase* "---abcd")
    => "---Abcd")

  (check
      (string-titlecase* "abcd efgh")
    => "Abcd Efgh")

  (check
      (string-titlecase* (view "greasy fried chicken" (start 2)))
    => "Easy Fried Chicken")

;;; --------------------------------------------------------------------

  (check
      (let* ((str (string-copy "abcd")))
	(string-titlecase*! str)
	str)
    => "Abcd")

  (check
      (let* ((str (string-copy "123abcd")))
	(string-titlecase*! str)
	str)
    => "123Abcd")

  (check
      (let* ((str (string-copy "---abcd")))
	(string-titlecase*! str)
	str)
    => "---Abcd")

  (check
      (let* ((str (string-copy "abcd efgh")))
	(string-titlecase*! str)
	str)
    => "Abcd Efgh")

  (check
      (let ((str (string-copy "greasy fried chicken")))
	(string-titlecase*! (view str (start 2)))
	str)
    => "grEasy Fried Chicken")

  )


(parameterise ((check-test-name 'folding))

  (check
      (string-fold-left (lambda (i nil x) (cons x nil)) '() "abcd")
    => '(#\d #\c #\b #\a))

  (check
      (string-fold-left (lambda (i nil x y) (cons (cons x y) nil)) '()
			"abcd"
			"ABCD")
    => '((#\d . #\D)
	 (#\c . #\C)
	 (#\b . #\B)
	 (#\a . #\A)))

  (check
      (string-fold-left (lambda (i nil x) (cons x nil)) '() "")
    => '())

  (check
      (string-fold-left (lambda (i count c)
			  (if (char-upper-case? c)
			      (+ count 1)
			    count))
			0
			"ABCdefGHi")
    => 5)

;;; --------------------------------------------------------------------

  (check
      (string-fold-right (lambda (i nil x) (cons x nil)) '() "abcd")
    => '(#\a #\b #\c #\d))

  (check
      (string-fold-right (lambda (i nil x y) (cons (cons x y) nil)) '()
			 "abcd"
			 "ABCD")
    => '((#\a . #\A)
	 (#\b . #\B)
	 (#\c . #\C)
	 (#\d . #\D)))

  (check
      (string-fold-right (lambda (i nil x) (cons x nil)) '() "")
    => '())

;;; --------------------------------------------------------------------

  (check
      (string-fold-left* (lambda (i nil x) (cons x nil)) '() "abcd")
    => '(#\d #\c #\b #\a))

  (check
      (string-fold-left* (lambda (i nil x y) (cons (cons x y) nil)) '()
			 "abcd"
			 "ABCDE")
    => '((#\d . #\D)
	 (#\c . #\C)
	 (#\b . #\B)
	 (#\a . #\A)))

  (check
      (string-fold-left* (lambda (i nil x) (cons x nil)) '() "")
    => '())

  (check
      (string-fold-left* (lambda (i count c)
			   (if (char-upper-case? c)
			       (+ count 1)
			     count))
			 0
			 "ABCdefGHi")
    => 5)

;;; --------------------------------------------------------------------

  (check
      (string-fold-right* (lambda (i nil x) (cons x nil)) '() "abcd")
    => '(#\a #\b #\c #\d))

  (check
      (string-fold-right* (lambda (i nil x y) (cons (cons x y) nil)) '()
			  "abcd"
			  "ABCDE")
    => '((#\a . #\A)
	 (#\b . #\B)
	 (#\c . #\C)
	 (#\d . #\D)))

  (check
      (string-fold-right* (lambda (i nil x) (cons x nil)) '() "")
    => '())

;;; --------------------------------------------------------------------

  (check
      (substring-fold-left cons '() "abcd")
    => '(#\d #\c #\b #\a))

  (check
      (substring-fold-left cons '() "")
    => '())

  (check
      (substring-fold-left (lambda (c count)
			     (if (char-upper-case? c)
				 (+ count 1)
			       count))
			   0
			   "ABCdefGHi")
    => 5)

  (check
      (let* ((str "abc\\de\\f\\ghi")
	     (ans-len (substring-fold-left
		       (lambda (c sum)
			 (+ sum (if (char=? c #\\) 2 1)))
		       0 str))
	     (ans (make-string ans-len)))
	(substring-fold-left
	 (lambda (c i)
	   (let ((i (if (char=? c #\\)
			(begin
			  (string-set! ans i #\\)
			  (+ i 1))
		      i)))
	     (string-set! ans i c)
	     (+ i 1)))
	 0 str)
	ans)
    => "abc\\\\de\\\\f\\\\ghi")

;;; --------------------------------------------------------------------

  (check
      (substring-fold-right cons '() "abcd")
    => '(#\a #\b #\c #\d))

  (check
      (substring-fold-right cons '() "")
    => '())

;;; --------------------------------------------------------------------

  (check
      (string-unfold null? car cdr '(#\a #\b #\c #\d))
    => "abcd")

  (check
      (string-unfold null? car cdr '())
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-unfold-right null? car cdr '(#\a #\b #\c #\d))
    => "dcba")

  (check
      (string-unfold-right null? car cdr '())
    => "")

  )


(parameterise ((check-test-name 'selecting))

  (check
      (string-take "abcd" 2)
    => "ab")

  (check
      (string-take "" 0)
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(string-take "abcd" 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (string-take-right "abcd" 2)
    => "cd")

  (check
      (string-take-right "" 0)
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(string-take-right "abcd" 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (string-drop "abcd" 2)
    => "cd")

  (check
      (string-drop "" 0)
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(string-drop "abcd" 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (string-drop-right "abcd" 2)
    => "ab")

  (check
      (string-drop-right "" 0)
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(string-drop-right "abcd" 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (string-trim "aaabcd" #\a)
    => "bcd")

  (check
      (string-trim "bcd" #\a)
    => "bcd")

  (check
      (string-trim "" #\a)
    => "")

  (check
      (string-trim "aaabcd" (char-set #\a #\b))
    => "cd")

  (check
      (string-trim "bcd" (char-set #\a #\b))
    => "cd")

  (check
      (string-trim "" (char-set #\a #\b))
    => "")

  (check
      (string-trim "AAAbcd" char-upper-case?)
    => "bcd")

  (check
      (string-trim "bcd" char-upper-case?)
    => "bcd")

  (check
      (string-trim "" char-upper-case?)
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-trim-right "bcdaaa" #\a)
    => "bcd")

  (check
      (string-trim-right "bcd" #\a)
    => "bcd")

  (check
      (string-trim-right "" #\a)
    => "")

  (check
      (string-trim-right "cdbaaa" (char-set #\a #\b))
    => "cd")

  (check
      (string-trim-right "cdb" (char-set #\a #\b))
    => "cd")

  (check
      (string-trim-right "" (char-set #\a #\b))
    => "")

  (check
      (string-trim-right "bcdAAA" char-upper-case?)
    => "bcd")

  (check
      (string-trim-right "bcd" char-upper-case?)
    => "bcd")

  (check
      (string-trim-right "" char-upper-case?)
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-trim-both "aaabcdaaa" #\a)
    => "bcd")

  (check
      (string-trim-both "bcd" #\a)
    => "bcd")

  (check
      (string-trim-both "" #\a)
    => "")

  (check
      (string-trim-both "aaabcdaa" (char-set #\a #\b))
    => "cd")

  (check
      (string-trim-both "bcdb" (char-set #\a #\b))
    => "cd")

  (check
      (string-trim-both "" (char-set #\a #\b))
    => "")

  (check
      (string-trim-both "AAAbcdAAA" char-upper-case?)
    => "bcd")

  (check
      (string-trim-both "bcd" char-upper-case?)
    => "bcd")

  (check
      (string-trim-both "" char-upper-case?)
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-pad "abc" 3 #\0)
    => "abc")

  (check
      (string-pad "abc" 5 #\0)
    => "00abc")

  (check
      (string-pad "abc" 5)
    => "  abc")

  (check
      (string-pad "abc" 2 #\0)
    => "bc")

  (check
      (string-pad "abc" 0 #\0)
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-pad-right "abc" 3 #\0)
    => "abc")

  (check
      (string-pad-right "abc" 5 #\0)
    => "abc00")

  (check
      (string-pad-right "abc" 2 #\0)
    => "ab")

  (check
      (string-pad-right "abc" 0 #\0)
    => "")

  )


(parameterise ((check-test-name 'prefix))

  (check
      (string-prefix-length "abcdefg" "abcd123")
    => 4)

  (check
      (string-prefix-length "aBcdefg" "abcd123")
    => 1)

  (check
      (string-prefix-length "efg" "123")
    => 0)

  (check
      (string-prefix-length "a" "a")
    => 1)

  (check
      (string-prefix-length "1" "2")
    => 0)

  (check
      (string-prefix-length "" "abcd123")
    => 0)

  (check
      (string-prefix-length "abcdefg" "")
    => 0)

;;; --------------------------------------------------------------------

  (check
      (string-suffix-length "efgabcd" "123abcd")
    => 4)

  (check
      (string-suffix-length "efgabcd" "123abCd")
    => 1)

  (check
      (string-suffix-length "efg" "123")
    => 0)

  (check
      (string-suffix-length "a" "a")
    => 1)

  (check
      (string-suffix-length "1" "2")
    => 0)

  (check
      (string-suffix-length "" "abcd123")
    => 0)

  (check
      (string-suffix-length "abcdefg" "")
    => 0)

;;; --------------------------------------------------------------------

  (check
      (string-prefix-length-ci "aBcdefg" "aBcd123")
    => 4)

  (check
      (string-prefix-length-ci "aBcdefg" "abcd123")
    => 4)

  (check
      (string-prefix-length-ci "efg" "123")
    => 0)

  (check
      (string-prefix-length-ci "a" "a")
    => 1)

  (check
      (string-prefix-length-ci "1" "2")
    => 0)

  (check
      (string-prefix-length-ci "" "abcd123")
    => 0)

  (check
      (string-prefix-length-ci "abcdefg" "")
    => 0)

;;; --------------------------------------------------------------------

  (check
      (string-suffix-length-ci "efgabCd" "123abCd")
    => 4)

  (check
      (string-suffix-length-ci "efgabCd" "123abcd")
    => 4)

  (check
      (string-suffix-length-ci "efg" "123")
    => 0)

  (check
      (string-suffix-length-ci "a" "a")
    => 1)

  (check
      (string-suffix-length-ci "1" "2")
    => 0)

  (check
      (string-suffix-length-ci "" "abcd123")
    => 0)

  (check
      (string-suffix-length-ci "abcdefg" "")
    => 0)

;;; --------------------------------------------------------------------

  (check
      (string-prefix? "abcd" "abcd123")
    => #t)

  (check
      (string-prefix? "abcd" "aBcd123")
    => #f)

  (check
      (string-prefix? "efg" "123")
    => #f)

  (check
      (string-prefix? "" "123")
    => #t)

  (check
      (string-prefix? "efg" "")
    => #f)

  (check
      (string-prefix? "" "")
    => #t)

;;; --------------------------------------------------------------------

  (check
      (string-prefix-ci? "aBcd" "aBcd123")
    => #t)

  (check
      (string-prefix-ci? "abcd" "aBcd123")
    => #t)

  (check
      (string-prefix-ci? "efg" "123")
    => #f)

  (check
      (string-prefix-ci? "" "123")
    => #t)

  (check
      (string-prefix-ci? "efg" "")
    => #f)

  (check
      (string-prefix-ci? "" "")
    => #t)

;;; --------------------------------------------------------------------

  (check
      (string-suffix? "abcd" "123abcd")
    => #t)

  (check
      (string-suffix? "abcd" "123aBcd")
    => #f)

  (check
      (string-suffix? "efg" "123")
    => #f)

  (check
      (string-suffix? "" "123")
    => #t)

  (check
      (string-suffix? "efg" "")
    => #f)

  (check
      (string-suffix? "" "")
    => #t)

;;; --------------------------------------------------------------------

  (check
      (string-suffix-ci? "aBcd" "123aBcd")
    => #t)

  (check
      (string-suffix-ci? "abcd" "123aBcd")
    => #t)

  (check
      (string-suffix-ci? "efg" "123")
    => #f)

  (check
      (string-suffix-ci? "" "123")
    => #t)

  (check
      (string-suffix-ci? "efg" "")
    => #f)

  (check
      (string-suffix-ci? "" "")
    => #t)

  )


(parameterise ((check-test-name 'searching))

  (check
      (string-index "abcd" #\b)
    => 1)

  (check
      (string-index (view "abcd" (start 1)) #\b)
    => 1)

  (check
      (string-index "abcd" #\1)
    => #f)

  (check
      (string-index "" #\1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-index "abcd" (char-set #\b #\B))
    => 1)

  (check
      (string-index (view "abcd" (start 1)) (char-set #\b #\B))
    => 1)

  (check
      (string-index "abcd" (char-set #\0 #\1))
    => #f)

  (check
      (string-index "" (char-set #\0 #\1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-index "aBcd" char-upper-case?)
    => 1)

  (check
      (string-index (view "aBcd" (start 1)) char-upper-case?)
    => 1)

  (check
      (string-index "abcd" char-upper-case?)
    => #f)

  (check
      (string-index "" char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-index-right "abcd" #\b)
    => 1)

  (check
      (string-index-right (view "abcd" (start 1)) #\b)
    => 1)

  (check
      (string-index-right "abcd" #\1)
    => #f)

  (check
      (string-index-right "" #\1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-index-right "abcd" (char-set #\b #\B))
    => 1)

  (check
      (string-index-right (view "abcd" (start 1)) (char-set #\b #\B))
    => 1)

  (check
      (string-index-right "abcd" (char-set #\0 #\1))
    => #f)

  (check
      (string-index-right "" (char-set #\0 #\1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-index-right "aBcd" char-upper-case?)
    => 1)

  (check
      (string-index-right (view "aBcd" (start 1)) char-upper-case?)
    => 1)

  (check
      (string-index-right "abcd" char-upper-case?)
    => #f)

  (check
      (string-index-right "" char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-skip "bacd" #\b)
    => 1)

  (check
      (string-skip (view "bacd" (start 1)) #\b)
    => 1)

  (check
      (string-skip "1111" #\1)
    => #f)

  (check
      (string-skip "" #\1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-skip "bacd" (char-set #\b #\B))
    => 1)

  (check
      (string-skip (view "bacd" (start 1)) (char-set #\b #\B))
    => 1)

  (check
      (string-skip "1010" (char-set #\0 #\1))
    => #f)

  (check
      (string-skip "" (char-set #\0 #\1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-skip "Bacd" char-upper-case?)
    => 1)

  (check
      (string-skip (view "Bacd" (start 1)) char-upper-case?)
    => 1)

  (check
      (string-skip "ABCD" char-upper-case?)
    => #f)

  (check
      (string-skip "" char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-skip-right "acdb" #\b)
    => 2)

  (check
      (string-skip-right (view "acdb" (start 1)) #\b)
    => 2)

  (check
      (string-skip-right "1111" #\1)
    => #f)

  (check
      (string-skip-right "" #\1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-skip-right "acdb" (char-set #\b #\B))
    => 2)

  (check
      (string-skip-right (view "acdb" (start 1)) (char-set #\b #\B))
    => 2)

  (check
      (string-skip-right "0101" (char-set #\0 #\1))
    => #f)

  (check
      (string-skip-right "" (char-set #\0 #\1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-skip-right "acdB" char-upper-case?)
    => 2)

  (check
      (string-skip-right (view "acdB" (start 1)) char-upper-case?)
    => 2)

  (check
      (string-skip-right "ABCD" char-upper-case?)
    => #f)

  (check
      (string-skip-right "" char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-count "abcbd" #\b)
    => 2)

  (check
      (string-count (view "abcd" (start 1)) #\b)
    => 1)

  (check
      (string-count "abcd" #\1)
    => 0)

  (check
      (string-count "" #\1)
    => 0)

;;; --------------------------------------------------------------------

  (check
      (string-count "abcBd" (char-set #\b #\B))
    => 2)

  (check
      (string-count (view "abcd" (start 1)) (char-set #\b #\B))
    => 1)

  (check
      (string-count "abcd" (char-set #\0 #\1))
    => 0)

  (check
      (string-count "" (char-set #\0 #\1))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (string-count "aBcAd" char-upper-case?)
    => 2)

  (check
      (string-count (view "aBcd" (start 1)) char-upper-case?)
    => 1)

  (check
      (string-count "abcd" char-upper-case?)
    => 0)

  (check
      (string-count "" char-upper-case?)
    => 0)

;;; --------------------------------------------------------------------

  (check
      (string-contains "ciao hello salut" "hello")
    => 5)

  (check
      (string-contains "ciao hello salut" "hola")
    => #f)

  (check
      (string-contains "ciao hello salut" "")
    => 0)

  (check
      (string-contains "" "hello")
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-contains-ci "ciAO HELLO saLUT" "hello")
    => 5)

  (check
      (string-contains-ci "ciao hello salut" "HOLA")
    => #f)

  (check
      (string-contains-ci "ciao hello salut" "")
    => 0)

  (check
      (string-contains-ci "" "hello")
    => #f)

  )


(parameterise ((check-test-name 'searching-and-replacing))

  (check	;no replacing because of no match
      (let ((src "abcdabcdabc")
	    (ptn "A")
	    (rep "12345"))
	(string-search-and-replace src ptn rep +inf.0))
    => "abcdabcdabc")

  (check	;replacing 3 chars
      (let ((src "AbcdAbcdAbc")
	    (ptn "A")
	    (rep "12345"))
	(string-search-and-replace src ptn rep +inf.0))
    => "12345bcd12345bcd12345bc")

  (check	;replacing 3 substrings
      (let ((src "bcABCbcABCbcdpqstABCbc")
	    (ptn "ABC")
	    (rep "12345"))
	(string-search-and-replace src ptn rep +inf.0))
    => "bc12345bc12345bcdpqst12345bc")

;;; --------------------------------------------------------------------

  (check	;source string range
      (let ((src "bcABCbcdABCbcdABCbc")
	    (ptn "ABC")
	    (rep "12345"))
	(string-search-and-replace (view src (start 2) (past (- (string-length src) 2)))
				   ptn rep +inf.0))
    => "12345bcd12345bcd12345")

  (check	;pattern range
      (let ((src "bcABCbcdABCbcdABCbc")
	    (ptn "pqABCst")
	    (rep "12345"))
	(string-search-and-replace src (view ptn (start 2) (past 5))
				   rep +inf.0))
    => "bc12345bcd12345bcd12345bc")

  (check	;replacement range
      (let ((src "bcABCbcdABCbcdABCbc")
	    (ptn "ABC")
	    (rep "pq12345st"))
	(string-search-and-replace src ptn (view rep (start 2) (past 7)) +inf.0))
    => "bc12345bcd12345bcd12345bc")

;;; --------------------------------------------------------------------

  (check	;zero replacements count
      (let ((src "bcABCbcdABCbcdABCbc")
	    (ptn "ABC")
	    (rep "12345"))
	(string-search-and-replace src ptn rep 0))
    => "bcABCbcdABCbcdABCbc")

  (check	;one replacement count
      (let ((src "bcABCbcdABCbcdABCbc")
	    (ptn "ABC")
	    (rep "12345"))
	(string-search-and-replace src ptn rep 1))
    => "bc12345bcdABCbcdABCbc")

  (check	;two replacements count
      (let ((src "bcABCbcdABCbcdABCbc")
	    (ptn "ABC")
	    (rep "12345"))
	(string-search-and-replace src ptn rep 2))
    => "bc12345bcd12345bcdABCbc")

  (check	;three replacements count
      (let ((src "bcABCbcdABCbcdABCbc")
	    (ptn "ABC")
	    (rep "12345"))
	(string-search-and-replace src ptn rep 3))
    => "bc12345bcd12345bcd12345bc")

  #t)


(parameterise ((check-test-name 'filtering))

  (check
      (string-delete "abcbd" #\b)
    => "acd")

  (check
      (string-delete "abcbd" #\0)
    => "abcbd")

  (check
      (string-delete "" #\b)
    => "")

  (check
      (string-delete "bbb" #\b)
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-delete "abcbd" (char-set #\b #\B))
    => "acd")

  (check
      (string-delete "abcbd" (char-set #\0 #\1))
    => "abcbd")

  (check
      (string-delete "" (char-set #\b #\B))
    => "")

  (check
      (string-delete "BbB" (char-set #\b #\B))
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-delete "aBcBd" char-upper-case?)
    => "acd")

  (check
      (string-delete "abcbd" char-upper-case?)
    => "abcbd")

  (check
      (string-delete "" char-upper-case?)
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-filter "abcbd" #\b)
    => "bb")

  (check
      (string-filter "abcbd" #\0)
    => "")

  (check
      (string-filter "" #\b)
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-filter "abcbd" (char-set #\b #\B))
    => "bb")

  (check
      (string-filter "abcbd" (char-set #\0 #\1))
    => "")

  (check
      (string-filter "" (char-set #\b #\B))
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-filter "aBcBd" char-upper-case?)
    => "BB")

  (check
      (string-filter "abcbd" char-upper-case?)
    => "")

  (check
      (string-filter "" char-upper-case?)
    => "")

  )


(parameterise ((check-test-name 'lists))

  (check
      (string->list* "abcd")
    => '(#\a #\b #\c #\d))

  (check
      (string->list* (view "abcd" (start 1) (past 3)))
    => '(#\b #\c))

  (check
      (string->list* "")
    => '())

;;; --------------------------------------------------------------------

  (check
      (reverse-list->string '(#\a #\b #\c #\d))
    => "dcba")

  (check
      (reverse-list->string '())
    => "")

  )

;;; --------------------------------------------------------------------

(parameterise ((check-test-name 'tokenize))

  (check
      (string-tokenize "ciao hello salut"
		       (char-set #\a #\c #\e #\i #\h #\l #\o #\s #\t #\u))
    => '("ciao" "hello" "salut"))

  (check
      (string-tokenize "" (char-set #\a #\c #\e #\i #\h #\l #\o #\s #\t #\u))
    => '())

  (check
      (string-tokenize "ciao hello salut" (char-set))
    => '())

  (check
      (string-tokenize "Help make programs run, run, RUN!"
		       (char-set-complement (char-set #\space)
					    char-set:ascii))
    => '("Help" "make" "programs" "run," "run," "RUN!"))

  #f)

;;; --------------------------------------------------------------------

(parameterise ((check-test-name 'join))

  (check
      (string-join '("c" "i" "a" "o") "," 'infix)
    => "c,i,a,o")

  (check
      (string-join '("c" "i" "a" "o") "," 'strict-infix)
    => "c,i,a,o")

  (check
      (string-join '("c" "i" "a" "o") "," 'suffix)
    => "c,i,a,o,")

  (check
      (string-join '("c" "i" "a" "o") "," 'prefix)
    => ",c,i,a,o")

;;; --------------------------------------------------------------------

  (check
      (string-join '() "," 'infix)
    => "")

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(string-join '() "," 'strict-infix))
    => #t)

  (check
      (string-join '() "," 'suffix)
    => "")

  (check
      (string-join '() "," 'prefix)
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-join '("c") "," 'infix)
    => "c")

  (check
      (string-join '("c") "," 'strict-infix)
    => "c")

  (check
      (string-join '("c") "," 'suffix)
    => "c,")

  (check
      (string-join '("c") "," 'prefix)
    => ",c")

;;; --------------------------------------------------------------------

  (check
      (string-join '("") "," 'infix)
    => "")

  (check
      (string-join '("") "," 'strict-infix)
    => "")

  (check
      (string-join '("") "," 'suffix)
    => ",")

  (check
      (string-join '("") "," 'prefix)
    => ",")

;;; --------------------------------------------------------------------

  (check
      (string-join '("c" "i" "a" "o") "" 'infix)
    => "ciao")

  (check
      (string-join '("c" "i" "a" "o") "" 'strict-infix)
    => "ciao")

  (check
      (string-join '("c" "i" "a" "o") "" 'suffix)
    => "ciao")

  (check
      (string-join '("c" "i" "a" "o") "" 'prefix)
    => "ciao")

;;; --------------------------------------------------------------------

  (check
      (string-join '("c" "i" "a" "o") ",;;" 'infix)
    => "c,;;i,;;a,;;o")

  (check
      (string-join '("c" "i" "a" "o") ",;;" 'strict-infix)
    => "c,;;i,;;a,;;o")

  (check
      (string-join '("c" "i" "a" "o") ",;;" 'suffix)
    => "c,;;i,;;a,;;o,;;")

  (check
      (string-join '("c" "i" "a" "o") ",;;" 'prefix)
    => ",;;c,;;i,;;a,;;o")

  )


(parameterise ((check-test-name 'xsubstring))

  (check
      (xsubstring "ciao " 0 5)
    => "ciao ")

  (check
      (xsubstring "ciao " 0 9)
    => "ciao ciao")

  (check
      (xsubstring "ciao " -5 5)
    => "ciao ciao ")

  (check
      (xsubstring "ciao " 2 4)
    => "ao")

  (check
      (xsubstring "ciao " -3 7)
    => "ao ciao ci")

  (check (xsubstring "abcdef" 1 7) => "bcdefa")
  (check (xsubstring "abcdef" 2 8) => "cdefab")
  (check (xsubstring "abcdef" 3 9) => "defabc")
  (check (xsubstring "abcdef" 4 10) => "efabcd")
  (check (xsubstring "abcdef" 5 11) => "fabcde")

  (check (xsubstring "abcdef" -1 5) => "fabcde")
  (check (xsubstring "abcdef" -2 4) => "efabcd")
  (check (xsubstring "abcdef" -3 3) => "defabc")
  (check (xsubstring "abcdef" -4 2) => "cdefab")
  (check (xsubstring "abcdef" -5 1) => "bcdefa")

  (check
      (xsubstring "ciao " 3 3)
    => "")

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(xsubstring "" 0 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((result (string-copy "01234")))
	(string-xcopy! result "ciao " 0 5)
	result)
    => "ciao ")

  (check
      (let ((result (string-copy "012345678")))
	(string-xcopy! result "ciao " 0 9)
	result)
    => "ciao ciao")

  (check
      (let ((result (string-copy "0123456789")))
	(string-xcopy! result "ciao " -5 5)
	result)
    => "ciao ciao ")

  (check
      (let ((result (string-copy "01")))
	(string-xcopy! result "ciao " 2 4)
	result)
    => "ao")

  (check
      (let ((result (string-copy "0123456789")))
	(string-xcopy! result "ciao " -3 7)
	result)
    => "ao ciao ci")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	  (string-xcopy! "" "" 0 5))
    => #t)

  )


(parameterise ((check-test-name 'filling))

  (check
      (let* ((str (string-copy "abcd")))
	(string-fill*! str #\b)
	str)
    => "bbbb")

  (check
      (let* ((str (string-copy "accd")))
	(string-fill*! (view str (start 1) (past 3)) #\b)
	str)
    => "abbd")

  (check
      (let* ((str (string-copy "")))
	(string-fill*! (view str (start 0) (past 0)) #\b)
	str)
    => "")

  )


(parameterise ((check-test-name 'reverse))

  (check
      (string-reverse "abcd")
    => "dcba")

  (check
      (string-reverse "")
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str (string-copy "abcd")))
	(string-reverse! str)
	str)
    => "dcba")

  (check
      (let* ((str (string-copy "")))
	(string-reverse! str)
	str)
    => "")

  )


(parameterise ((check-test-name 'replace))

  (check
      (string-replace "abcd" "1234")
    => "1234")

  (check
      (string-replace (view "abcd" (start 2) (past 2)) "1234")
    => "ab1234cd")

  (check
      (string-replace (view "abcd" (start 2) (past 2)) "")
    => "abcd")

  (check
      (string-replace (view "abcd" (start 1) (past 3)) "1234")
    => "a1234d")

  (check
      (string-replace (view "abcd" (start 0) (past 3)) "1234")
    => "1234d")

  (check
      (string-replace (view "abcd" (start 1) (past 4)) "1234")
    => "a1234")

  )


(parameterise ((check-test-name 'mutating))

  (check
      (let* ((str (string-copy "12")))
	;; not enough room in destination string
	(guard (exc ((assertion-violation? exc) #t))
	  (string-copy*! (view str (start 3))
			 (view "abcd" (past 2)))))
    => #t)

  (check
      ;; whole string copy
      (let* ((str (string-copy "123")))
	(string-copy*! str "abc")
	str)
    => "abc")

  (check
      ;; zero-elements string copy
      (let* ((str (string-copy "123")))
	(string-copy*! str (view "abc" (start 2) (past 2)))
	str)
    => "123")

  (check
      ;; one-element string copy
      (let* ((str (string-copy "123")))
	(string-copy*! str (view "abc" (start 1) (past 2)))
	str)
    => "b23")

  (check
      ;; two-elements string copy
      (let* ((str (string-copy "12")))
	(string-copy*! str (view "abcd" (past 2)))
	str)
    => "ab")

  (check
      (let ((str ""))
	(string-copy*! str (view "abcd" (start 0) (past 0)))
	str)
    => "")

  (check
      ;; over the same string, full
      (let* ((str (string-copy "0123456789")))
	(string-copy*! str str)
	str)
    => "0123456789")

  (check
      ;; over the same string, in place
      (let* ((str (string-copy "0123456789")))
	(string-copy*! (view str (start 5)) (view str (start 5)))
	str)
    => "0123456789")

  (check
      ;; over the same string, backwards
      (let* ((str (string-copy "0123456789")))
	(string-copy*! (view str (start 2))
		       (view str (start 4) (past 8)))
	str)
    => "0145676789")

  (check
      ;; over the same string, backwards
      (let* ((str (string-copy "0123456789")))
	(string-copy*! (view str (start 0))
		       (view str (start 4) (past 8)))
	str)
    => "4567456789")

  (check
      ;; over the same string, forwards
      (let* ((str (string-copy "0123456789")))
	(string-copy*! (view str (start 4))
		       (view str (start 2) (past 6)))
	str)
    => "0123234589")

  (check
      ;; over the same string, forwards
      (let* ((str (string-copy "0123456789")))
	(string-copy*! (view str (start 6))
		       (view str (start 2) (past 6)))
	str)
    => "0123452345")

;;; --------------------------------------------------------------------

  (check
      (let* ((str (string-copy "12")))
	;; not enough room in destination string
	;;(string-reverse-copy*! (str 3) (view '#(#\a #\b #\c #\d) (past 2)))
	(guard (exc ((assertion-violation? exc) #t))
	  (string-reverse-copy*! (view str (start 3))
				 (view "abcd" (past 2)))))
    => #t)

  (check
      ;; whole string copy
      (let* ((str (string-copy "123")))
	(string-reverse-copy*! str "abc")
	str)
    => "cba")

  (check
      ;; zero-elements string copy
      (let* ((str (string-copy "123")))
	(string-reverse-copy*! str (view "abc" (start 2) (past 2)))
	str)
    => "123")

  (check
      ;; one-element string copy
      (let* ((str (string-copy "123")))
	(string-reverse-copy*! str (view "abc" (start 1) (past 2)))
	str)
    => "b23")

  (check
      ;; two-elements string copy
      (let* ((str (string-copy "12")))
	(string-reverse-copy*! str (view "abcd" (past 2)))
	str)
    => "ba")

  (check
      (let ((str ""))
	(string-reverse-copy*! str (view "abcd" (start 0) (past 0)))
	str)
    => "")

  (check
      ;; over the same string, full
      (let* ((str (string-copy "0123456789")))
	(string-reverse-copy*! str str)
	str)
    => "9876543210")

  (check
      ;; over the same string
      (let* ((str (string-copy "0123456789")))
	(string-reverse-copy*! (view str (start 5))
			       (view str (start 5)))
	str)
    => "0123498765")

  (check
      ;; over the same string, backwards
      (let* ((str (string-copy "0123456789")))
	(string-reverse-copy*! (view str (start 2))
			       (view str (start 4) (past 8)))
	str)
    => "0176546789")

  (check
      ;; over the same string, backwards
      (let* ((str (string-copy "0123456789")))
	(string-reverse-copy*! (view str (start 0))
			       (view str (start 4) (past 8)))
	str)
    => "7654456789")

  (check
      ;; over the same string, forwards
      (let* ((str (string-copy "0123456789")))
	(string-reverse-copy*! (view str (start 4))
			       (view str (start 2) (past 6)))
	str)
    => "0123543289")

  (check
      ;; over the same string, forwards
      (let* ((str (string-copy "0123456789")))
	(string-reverse-copy*! (view str (start 6))
			       (view str (start 2) (past 6)))
	str)
    => "0123455432")

;;; --------------------------------------------------------------------

  (check
      (let ((str (string-copy "012345")))
	(string-swap! str 2 4)
	str)
    => "014325")

  (check
      (let ((str (string-copy "012345")))
	(string-swap! str 2 2)
	str)
    => "012345")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(string-swap! "" 0 1))
    => #t)

  )


(parameterise ((check-test-name 'misc))

  (check
      (let* ((line	"ciao ciao hello salut ciao salut")
	     (words	(string-tokenize line char-set:ascii/letter))
	     (getter	(let ((words words))
			  (lambda ()
			    (if (null? words)
				#f
			      (begin0
				  (car words)
				(set! words (cdr words)))))))
	     (result	(word-frequency getter)))
	(list (hashtable-ref result "ciao" 0)
	      (hashtable-ref result "hello" 0)
	      (hashtable-ref result "salut" 0)))
    => '(3 1 2))

  #t)


;;;; done

(check-report)

;;; end of file
