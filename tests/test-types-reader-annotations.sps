;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for reader annotation objects, typed language
;;;Date: Tue Dec 22, 2015
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
(program (test-vicare-records-typed)
  (options typed-language)
  (import (vicare)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: reader annotation objects with typed language\n")


(parametrise ((check-test-name	'reader-annotations))

  (check
      (let* ((port	(open-string-input-port "123"))
	     (datum	(get-annotated-datum port)))
	(is-a? datum <reader-annotation>))
    => #t)


  ;;Method call early binding.
  ;;
  (check
      (let* ((port				(open-string-input-port "123"))
	     ({datum <reader-annotation>}	(get-annotated-datum port)))
	#;(reader-annotation-stripped datum)
	(.stripped datum))
    => 123)

  ;;Method call late binding.
  ;;
  ;;FIXME To be uncommented  at the next boot image rotation.   (Marco Maggi; Sat Dec
  ;;26, 2015)
  ;;
  ;; (check
  ;;     (let* ((port		(open-string-input-port "123"))
  ;; 	     ({datum <top>}	(get-annotated-datum port)))
  ;; 	(method-call-late-binding 'stripped datum))
  ;;   => 123)

  #t)


;;;; done

(collect 'fullest)
(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
