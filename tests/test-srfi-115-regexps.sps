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
  (srfi :115))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: SRFI 115, regexps\n")


;;;; helpers

(define-syntax-rule (check-for-regexp-match ?body)
  (check-for-true
   (regexp-match? ?body)))


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
  (check-for-regexp-match
   (regexp-search '(w/nocase (~ ("Aab"))) "B"))
  (check-for-regexp-match
   (regexp-search '(w/nocase (~ ("Aab"))) "b"))
  (check-for-regexp-match
   (regexp-search '(~ (w/nocase ("Aab"))) "B"))
  (check-for-regexp-match
   (regexp-search '(~ (w/nocase ("Aab"))) "b"))

  #t)


;;;; done

(collect 4)
(check-report)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
