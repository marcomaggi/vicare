;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the <transcoder> type
;;;Date: Tue Oct 20, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(program (test-types-objects-transcoder)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) expander::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: tests for <transcoder> objects\n")


(parametrise ((check-test-name		'predicate))

  (check-for-true	(is-a? (native-transcoder) <transcoder>))
  (check-for-false	(is-a? 123 <transcoder>))

  (check-for-true	(let (({O <transcoder>} (native-transcoder)))
  			  (is-a? O <transcoder>)))

  (check-for-true	(let (({O <top>} (native-transcoder)))
  			  (is-a? O <transcoder>)))

  (check-for-false	(let (({O <top>} "ciao"))
  			  (is-a? O <transcoder>)))

  (void))


(parametrise ((check-test-name		'constructor))

  (check
      (transcoder? (new <transcoder> (utf-8-codec)))
    => #t)

  (check
      (transcoder? (new <transcoder> (utf-8-codec) (eol-style lf)))
    => #t)

  (check
      (transcoder? (new <transcoder> (utf-8-codec) (eol-style lf) (error-handling-mode ignore)))
    => #t)

  (check
      (expander::type-signature.syntax-object (type-of (new <transcoder> (read) (read) (read))))
    (=> expander::syntax=?)
    (list #'<transcoder>))

  (void))


(parametrise ((check-test-name		'methods))

  (check
      (let (({O <transcoder>} (new <transcoder> (utf-8-codec) (eol-style lf) (error-handling-mode ignore))))
	(.codec O))
    => (utf-8-codec))

  (check
      (let (({O <transcoder>} (new <transcoder> (utf-8-codec) (eol-style lf) (error-handling-mode ignore))))
	(.eol-style O))
    => (eol-style lf))

  (check
      (let (({O <transcoder>} (new <transcoder> (utf-8-codec) (eol-style lf) (error-handling-mode ignore))))
	(.handling-mode O))
    => (error-handling-mode ignore))

  (void))


(parametrise ((check-test-name		'late-binding))

  (check
      (let (({O <transcoder>} (new <transcoder> (utf-8-codec) (eol-style lf) (error-handling-mode ignore))))
	(method-call-late-binding 'codec #f O))
    => (utf-8-codec))

  (check
      (let (({O <transcoder>} (new <transcoder> (utf-8-codec) (eol-style lf) (error-handling-mode ignore))))
	(method-call-late-binding 'eol-style #f O))
    => (eol-style lf))

  (check
      (let (({O <transcoder>} (new <transcoder> (utf-8-codec) (eol-style lf) (error-handling-mode ignore))))
	(method-call-late-binding 'handling-mode #f O))
    => (error-handling-mode ignore))

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
