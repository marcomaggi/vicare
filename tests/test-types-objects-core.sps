;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for built-in Scheme object types under typed language
;;;Date: Thu Sep 17, 2015
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
      (xp.type-signature.tags (type-of (new <top> (read))))
    (=> syntax=?)
    (list #'<top>))

;;; --------------------------------------------------------------------
;;; methods

  (check
      (fixnum? (.hash (unsafe-cast-signature (<top>) 123)))
    => #t)

  (check
      (fixnum? (method-call-late-binding 'hash (unsafe-cast-signature (<top>) 123)))
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
      (xp.type-signature.tags (type-of (new <void>)))
    (=> syntax=?)
    (list #'<void>))

  #t)


(parametrise ((check-test-name	'procedure))

;;; type predicate

  (check-for-true
   (is-a? display <procedure>))

  (check-for-true
   (let ((O display))
     (is-a? O <procedure>)))

  (check-for-true
   (let (({O <procedure>} display))
     (is-a? O <procedure>)))

  (check-for-false
   (is-a? 123 <procedure>))

;;; --------------------------------------------------------------------

  (check
      (let (({f <procedure>} (lambda (x) x)))
	(xp.type-signature.tags (type-of (f 1))))
    (=> syntax=?)
    #'<list>)

  (check
      (let (({f <procedure>} (unsafe-cast-signature (<procedure>) (lambda (x) x))))
	(xp.type-signature.tags (type-of f)))
    (=> syntax=?)
    (list #'<procedure>))

;;; --------------------------------------------------------------------
;;; <predicate>

  (check-for-true	(type-super-and-sub? <procedure> <predicate>))
  (check-for-false	(type-super-and-sub? <predicate> <procedure>))

  (check
      (xp.type-signature.tags (type-of (unsafe-cast-signature (<predicate>) (read))))
    (=> syntax=?)
    (list #'<predicate>))

  (check
      (xp.type-signature.tags (type-of ((unsafe-cast-signature (<predicate>) (read)) 123)))
    (=> syntax=?)
    (list #'<boolean>))

  ;;FIXME This test  is commented out because handling of  procedure types must still
  ;;be developed to make sense.  (Marco Maggi; Sun Feb 14, 2016)
  ;;
  ;; (check
  ;;     (let (({f <predicate>} (unsafe-cast-signature (<predicate>) (lambda (x) x))))
  ;; 	(xp.type-signature.tags (type-of f)))
  ;;   (=> syntax=?)
  ;;   (list #'<predicate>))

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
