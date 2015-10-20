;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for built-in Scheme object types under typed language
;;;Date: Thu Sep 17, 2015
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
(program (test-vicare-typed-language-scheme-objects)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) xp.)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: typed language, built-in Scheme object types\n")


(parametrise ((check-test-name	'top))

  (check
      (is-a? 123 <top>)
    => #t)

  (check
      (procedure? (is-a? _ <top>))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (new <top> 123)
    => 123)

  (check
      (xp.type-signature-tags (type-of (new <top> (read))))
    (=> syntax=?)
    (list #'<top>))

;;; --------------------------------------------------------------------
;;; methods

  (check
      (fixnum? (.hash (unsafe-cast <top> 123)))
    => #t)

  (check
      (fixnum? (method-call-late-binding 'hash (unsafe-cast <top> 123)))
    => #t)

  #t)


(parametrise ((check-test-name	'void))

;;; type predicate

  (check-for-true
   (is-a? (void) <void>))

  (check-for-true
   (let ((O '#!void))
     (is-a? O <void>)))

  (check-for-true
   (let (({O <void>} '#!void))
     (is-a? O <void>)))

  (check-for-false
   (is-a? 123 <void>))

;;; --------------------------------------------------------------------
;;; constructor

  (check
      (new <void>)
    => '#!void)

  (check
      (xp.type-signature-tags (type-of (new <void>)))
    (=> syntax=?)
    (list #'<void>))

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
