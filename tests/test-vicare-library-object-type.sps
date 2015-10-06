;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for built-in type <library>
;;;Date: Tue Oct  6, 2015
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
(program (test-vicare-library-object-type)
  (options typed-language)
  (import (vicare)
    (prefix (vicare libraries) libs.)
    (only (vicare libraries)
	  <library>)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: <library> object-type\n")


(parametrise ((check-test-name	'type-predicate))

  (check-for-false
   (is-a? 123 <library>))

  (check
      (let ((L (libs.find-library-by-reference '(vicare))))
        (is-a? L <library>))
    => #t)

  (check
      (let ((L (libs.find-library-by-reference '(vicare))))
        (is-a? L <top>))
    => #t)

  #t)


(parametrise ((check-test-name	'field-accessors))

  ;;The built-in library  "(vicare)" has a version  number, so here we  check for its
  ;;name.
  ;;
  (check
      (let (({L <library>} (libs.find-library-by-reference '(vicare))))
        (libs.library-reference->identifiers (.name L)))
    => '(vicare))

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
