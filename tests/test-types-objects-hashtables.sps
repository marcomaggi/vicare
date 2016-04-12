;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the <hashtable-type-spec> type
;;;Date: Mon Apr  4, 2016
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(program (test-types-hashtable-objects)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) expander::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: <hashtable-type-spec> objects\n")


(parametrise ((check-test-name	'super-and-sub))

  (check-for-true	(type-annotation-super-and-sub? (hashtable <symbol> <number>)
							(hashtable <symbol> <number>)))

  (check-for-true	(type-annotation-super-and-sub? (hashtable <real>   <string>)
							(hashtable <fixnum> <string>)))

  (check-for-true	(type-annotation-super-and-sub? (hashtable <string>   <real>)
							(hashtable <string> <fixnum>)))

  (check-for-false	(type-annotation-super-and-sub? (hashtable <real>    <string>)
							(hashtable <boolean> <string>)))

  (check-for-false	(type-annotation-super-and-sub? (hashtable <string>    <real>)
							(hashtable <string> <boolean>)))

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
