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

  #t)


(parametrise ((check-test-name	'pair))

;;; type predicate

  (check-for-true
   (is-a? '(1 . 2) <pair>))

  (check-for-true
   (let ((O '(1 . 2)))
     (is-a? O <pair>)))

  (check-for-true
   (let (({O <pair>} '(1 . 2)))
     (is-a? O <pair>)))

  (check-for-false
   (is-a? 123 <pair>))

;;; --------------------------------------------------------------------
;;; constructor

  (check
      (new <pair> 1 2)
    => '(1 . 2))

;;; --------------------------------------------------------------------
;;; expand-time methods call

  (check
      (.car (new <pair> 1 2))
    => 1)

  (check
      (.cdr (new <pair> 1 2))
    => 2)

;;; --------------------------------------------------------------------
;;; run-time methods call

  (check
      (method-call-late-binding 'car (new <pair> 1 2))
    => 1)

  (check
      (method-call-late-binding 'cdr (new <pair> 1 2))
    => 2)

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

  #t)


(parametrise ((check-test-name	'boolean))

;;; type predicate

  (check-for-true
   (is-a? #t <boolean>))

  (check-for-true
   (is-a? #f <boolean>))

  (check-for-true
   (let ((O '#t))
     (is-a? O <boolean>)))

  (check-for-true
   (let ((O '#f))
     (is-a? O <boolean>)))

  (check-for-true
   (let (({O <boolean>} '#t))
     (is-a? O <boolean>)))

  (check-for-false
   (is-a? 123 <boolean>))

;;; --------------------------------------------------------------------
;;; constructor

  ;; (check
  ;;     (new <boolean> #t)
  ;;   => '#t)

  ;; (check
  ;;     (new <boolean> #f)
  ;;   => '#f)

  ;; (check-for-procedure-signature-argument-violation
  ;;     (new <boolean> 123)
  ;;   => '(<boolean> 1 boolean? 123))

  #t)


(parametrise ((check-test-name	'ports))

;;; type predicates

  (check-for-true	(is-a? (current-input-port)	<port>))
  (check-for-true	(is-a? (current-output-port)	<port>))
  (check-for-true	(is-a? (current-error-port)	<port>))

  (check-for-true
   (let (({P <textual-output-port>} (current-error-port)))
     (is-a? P <port>)))

  (check-for-true
   (let (({P <textual-output-port>} (current-error-port)))
     (is-a? P <output-port>)))

  (check-for-true
   (let (({P <textual-output-port>} (current-error-port)))
     (is-a? P <textual-output-port>)))

  (check-for-false
   (let (({P <textual-output-port>} (current-error-port)))
     (is-a? P <input-port>)))

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
