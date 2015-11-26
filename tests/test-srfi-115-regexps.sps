;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for SRFI 115
;;;Date: Wed Jun 10, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(import (vicare)
  (vicare checks)
  (srfi :14)
  (srfi :115))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: SRFI 115, regexps\n")


;;;; helpers

(define-syntax-rule (check-for-regexp-match ?body)
  (check-for-true
   (regexp-match? ?body)))


(parametrise ((check-test-name	'regexp))

  (check-for-true
   (regexp? (regexp "[a-z]")))

  (check-for-true
   (regexp? (regexp "[a-z]*")))

  (check-for-true
   (regexp? (regexp '(* "[a-z]"))))

  #t)


(parametrise ((check-test-name	'rx))

  (check-for-true
   (regexp? (rx "[a-z]")))

  (check-for-true
   (regexp? (rx "[a-z]*")))

  (check-for-true
   (regexp? (rx (* "[a-z]"))))

  #t)


(parametrise ((check-test-name	'regexp-to-sre))

  (check
      (regexp->sre (rx (* "[a-z]")))
    => '(: (* "[a-z]")))

  #t)


(parametrise ((check-test-name	'basic-patterns))

  (check-for-regexp-match
   (regexp-search "needle" "hayneedlehay"))

  (check-for-false
   (regexp-search "needle" "haynEEdlehay"))

;;; --------------------------------------------------------------------

  (check-for-regexp-match
   (regexp-search '(: "one" space "two" space "three")
		  "one two three"))

;;; --------------------------------------------------------------------

  (check-for-regexp-match
   (regexp-search '(or "eeney" "meeney" "miney") "meeney"))

  (check-for-false
   (regexp-search '(or "eeney" "meeney" "miney") "moe"))

;;; --------------------------------------------------------------------

  (check-for-false
   (regexp-search "needle" "haynEEdlehay"))
  (check-for-regexp-match
   (regexp-search '(w/nocase "needle") "haynEEdlehay"))

  (check-for-regexp-match
   (regexp-search '(~ ("Aab")) "B"))
  (check-for-false
   (regexp-search '(~ ("Aab")) "b"))

  (check-for-false
   (regexp-search '(w/nocase (~ ("Aab"))) "B"))
  (check-for-false
   (regexp-search '(w/nocase (~ ("Aab"))) "b"))
  (check-for-false
   (regexp-search '(~ (w/nocase ("Aab"))) "B"))
  (check-for-false
   (regexp-search '(~ (w/nocase ("Aab"))) "b"))

  (check-for-regexp-match
   (regexp-search '(w/nocase "abc") "abc"))
  (check-for-regexp-match
   (regexp-search '(w/nocase "abc") "Abc"))
  (check-for-regexp-match
   (regexp-search '(w/nocase "abc") "aBc"))
  (check-for-regexp-match
   (regexp-search '(w/nocase "abc") "abC"))
  (check-for-regexp-match
   (regexp-search '(w/nocase "abc") "ABc"))
  (check-for-regexp-match
   (regexp-search '(w/nocase "abc") "aBC"))
  (check-for-regexp-match
   (regexp-search '(w/nocase "abc") "ABC"))

  (check-for-regexp-match
   (regexp-search '(w/nocase ("[abc]")) "B"))

  (check-for-regexp-match
   (regexp-search '(w/nocase ("[ABC]")) "b"))

;;; --------------------------------------------------------------------

  (check-for-regexp-match
   (regexp-search '(w/nocase "SMALL" (w/case "BIG"))
		  "smallBIGsmall"))
  (check-for-false
   (regexp-search '(w/nocase (~ (w/case ("Aab")))) "b"))

;;; --------------------------------------------------------------------

  (check-for-regexp-match
   (regexp-search '(w/ascii bos (* alpha) eos) "English"))

  (check-for-regexp-match
   (regexp-search '(w/ascii bos (* "[a-zA-Z]")) "English"))

  (check-for-regexp-match
   (regexp-search '(w/ascii (* "[a-zA-Z]") eos) "English"))

  (check-for-regexp-match
   (regexp-search '(w/ascii bos (* (/ "azAZ")) eos) "English"))

  (check-for-regexp-match
   (regexp-search '(* "[a-zA-Z]") "English"))

;;; --------------------------------------------------------------------

  (check-for-regexp-match
   (regexp-search '(w/unicode bos (* "[a-zA-Z]")) "English"))

  (check-for-regexp-match
   (regexp-search '(w/unicode (* "[a-zA-Z]") eos) "English"))

  (check-for-regexp-match
   (regexp-search '(w/unicode bos (* (/ "azAZ")) eos) "English"))

  (check-for-regexp-match
   (regexp-search '(w/unicode bos (* alpha) eos) "English"))

;;; --------------------------------------------------------------------

  (let ((number '($ (+ digit))))
    (check
	(cdr
	 (regexp-match->list
	  (regexp-search `(: ,number "-" ,number "-" ,number)
			 "555-867-5309")))
      => '("555" "867" "5309"))
    (check
	(cdr
	 (regexp-match->list
	  (regexp-search `(: ,number "-" (w/nocapture ,number) "-" ,number)
			 "555-867-5309")))
      => '("555" "5309")))

  #t)


(parametrise ((check-test-name	'repeating-patterns))

  (check-for-regexp-match
   (regexp-search '(: "match" (? "es") "!") "matches!"))

  (check-for-regexp-match
   (regexp-search '(: "match" (? "es") "!") "match!"))

  (check-for-false
   (regexp-search '(: "match" (? "es") "!") "matche!"))

;;; --------------------------------------------------------------------

  (check-for-regexp-match
   (regexp-search '(: "<" (* (~ #\>)) ">") "<html>"))

  (check-for-regexp-match
   (regexp-search '(: "<" (* (~ #\>)) ">") "<>"))

  (check-for-false
   (regexp-search '(: "<" (* (~ #\>)) ">") "<html"))

;;; --------------------------------------------------------------------

  (check-for-regexp-match
   (regexp-search '(: "<" (+ (~ #\>)) ">") "<html>"))

  (check-for-regexp-match
   (regexp-search '(: "<" (+ (~ #\>)) ">") "<a>"))

  (check-for-false
   (regexp-search '(: "<" (+ (~ #\>)) ">") "<>"))

;;; --------------------------------------------------------------------

  (check-for-regexp-match
   (regexp-search '(: "<" (>= 3 (~ #\>)) ">") "<table>"))

  (check-for-regexp-match
   (regexp-search '(: "<" (>= 3 (~ #\>)) ">") "<pre>"))

  (check-for-false
   (regexp-search '(: "<" (>= 3 (~ #\>)) ">") "<tr>"))

;;; --------------------------------------------------------------------

  (check-for-regexp-match
   (regexp-search '(: "<" (= 4 (~ #\>)) ">") "<html>"))

  ;;The regexp expects 4 chars between "<" and ">".
  (check-for-false
   (regexp-search '(: "<" (= 4 (~ #\>)) ">") "<table>"))

;;; --------------------------------------------------------------------

  (check-for-regexp-match
   (regexp-search '(: (= 3 (** 1 3 numeric) ".") (** 1 3 numeric))
		  "192.168.1.10"))

  (check-for-false
   (regexp-search '(: (= 3 (** 1 3 numeric) ".") (** 1 3 numeric))
		  "192.0168.1.10"))

  #t)


(parametrise ((check-test-name	'char-sets-patterns))

  (define char-set:vowels
    (string->char-set "aeiou"))

;;; --------------------------------------------------------------------

  (check-for-regexp-match
   (regexp-matches '(* #\-) "---"))

  (check-for-false
   (regexp-matches '(* #\-) "-_-"))

;;; --------------------------------------------------------------------

  (check
      (regexp-partition `(+ ,char-set:vowels) "vowels")
    => '("v" "o" "w" "e" "ls"))

;;; --------------------------------------------------------------------

  (check-for-regexp-match
   (regexp-matches '(* ("aeiou")) "oui"))

  (check-for-false
   (regexp-matches '(* ("aeiou")) "ouais"))

  ;;FIXME These work in Chibi but fail here.
  ;;
  (check-for-regexp-match
   (regexp-matches '(* ("e\x0301;")) "e\x0301;"))

  (check-for-false
   (regexp-matches '("e\x0301;") "e\x0301;"))

  (check-for-regexp-match
   (regexp-matches '("e\x0301;") "e"))

  (check-for-regexp-match
   (regexp-matches '("e\x0301;") "\x0301;"))

  (check-for-false
   (regexp-matches '("e\x0301;") "\x00E9;"))

;;; --------------------------------------------------------------------

  (check-for-regexp-match
   (regexp-matches '(* (/ "AZ09")) "R2D2"))

  (check-for-false
   (regexp-matches '(* (/ "AZ09")) "C-3PO"))

;;; --------------------------------------------------------------------

  (check-for-regexp-match
   (regexp-matches '(* (- (/ "az") ("aeiou"))) "xyzzy"))

  (check-for-false
   (regexp-matches '(* (- (/ "az") ("aeiou"))) "vowels"))

;;; --------------------------------------------------------------------

  (check-for-regexp-match
   (regexp-matches '(* (& (/ "az") (~ ("aeiou")))) "xyzzy"))

  (check-for-false
   (regexp-matches '(* (& (/ "az") (~ ("aeiou")))) "vowels"))

  #t)


(parametrise ((check-test-name	'boundary-patterns))

  (check-for-regexp-match
   (regexp-search '(: bow "foo") "foo"))

  (check-for-regexp-match
   (regexp-search '(: bow "foo") "foobar"))

  (check-for-false
   (regexp-search '(: bow "foo") ""))

  (check-for-false
   (regexp-search '(: bow "foo") "snafoo"))

  (check-for-regexp-match
   (regexp-search '(: "foo" eow) "foo"))

  (check-for-regexp-match
   (regexp-search '(: "foo" eow) "foo!"))

  (check-for-false
   (regexp-search '(: "foo" eow) "foobar"))

  #t)


(parametrise ((check-test-name	'look-around-patterns))

  ;;Not implemented in the reference implementation.
  ;;
  ;; (check-for-regexp-match
  ;;  (regexp-matches '(: "regular" (look-ahead " expression")
  ;; 		       " expression")
  ;; 		   "regular expression"))

  ;; (check-for-false
  ;;  (regexp-matches '(: "regular" (look-ahead " ") "expression")
  ;;                   "regular expression"))

  #t)


;;;; done

(collect 4)
(check-report)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
