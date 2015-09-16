;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for structs, typed language
;;;Date: Sat Sep 12, 2015
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
(program (test-vicare-structs-typed)
  (options typed-language)
  (import (vicare)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: structs with typed language\n")


(parametrise ((check-test-name	'generic-std-syntax))

  (let ()	;application syntax
    (define-struct alpha
      (a b c))

    (check
	(eq? (struct-type-descriptor alpha)
	     (type-descriptor alpha))
      => #t)

    (void))

  #t)


(parametrise ((check-test-name	'generic-maker-syntax))

  (define-struct alpha
    (a b c))

  (define-struct beta
    (a b))

  (check
      (let ((stru (new alpha 1 2 3)))
	(alpha? stru))
    => #t)

  (check
      (let ((stru (new beta 1 2)))
	(beta? stru))
    => #t)

  (void))


(parametrise ((check-test-name	'generic-predicate-syntax))

  (define-struct alpha
    (a b c))

  (define-struct beta
    (a b c))

  (check
      (let ((stru (make-alpha 1 2 3)))
	(is-a? stru alpha))
    => #t)

  (check
      (let ((stru (make-alpha 1 2 3)))
	(is-a? stru beta))
    => #f)

  (check
      (let ((stru (make-alpha 1 2 3)))
	((is-a? _ alpha) stru))
    => #t)

  (check
      (is-a? 123 alpha)
    => #f)

  (check
      (is-a? 123 beta)
    => #f)

  (void))


(parametrise ((check-test-name	'generic-slots-syntax))

  (define-struct alpha
    (a b c))

;;; --------------------------------------------------------------------
;;; accessors and mutators

  (check
      (let ((stru (new alpha 1 2 3)))
	(list (slot-ref stru a alpha)
	      (slot-ref stru b alpha)
	      (slot-ref stru c alpha)))
    => '(1 2 3))

  (check
      (let ((stru (new alpha 1 2 3)))
	(slot-set! stru a alpha 19)
	(slot-set! stru b alpha 29)
	(slot-set! stru c alpha 39)
	(list (slot-ref stru a alpha)
	      (slot-ref stru b alpha)
	      (slot-ref stru c alpha)))
    => '(19 29 39))

  (check
      (let ((stru (new alpha 1 2 3)))
	(list ((slot-ref _ a alpha) stru)
	      ((slot-ref _ b alpha) stru)
	      ((slot-ref _ c alpha) stru)))
    => '(1 2 3))

  (check
      (let ((stru (new alpha 1 2 3)))
	((slot-set! _ a alpha _) stru 19)
	((slot-set! _ b alpha _) stru 29)
	((slot-set! _ c alpha _) stru 39)
	(list ((slot-ref _ a alpha) stru)
	      ((slot-ref _ b alpha) stru)
	      ((slot-ref _ c alpha) stru)))
    => '(19 29 39))

;;; --------------------------------------------------------------------

  (check
      (let (({stru alpha} (new alpha 1 2 3)))
	(list (slot-ref stru a)
	      (slot-ref stru b)
	      (slot-ref stru c)))
    => '(1 2 3))

  (check
      (let (({stru alpha} (new alpha 1 2 3)))
	(slot-set! stru a 19)
	(slot-set! stru b 29)
	(slot-set! stru c 39)
	(list (slot-ref stru a)
	      (slot-ref stru b)
	      (slot-ref stru c)))
    => '(19 29 39))

  #t)


(parametrise ((check-test-name	'methods))

  ;;Field accessors.
  ;;
  (check
      (internal-body

	(define-struct alpha
	  (a b))

	(define {O alpha}
	  (make-alpha 1 2))

	(values (method-call a O)
		(method-call b O)))
    => 1 2)

  ;;Field accessors and mutators.
  ;;
  (check
      (internal-body

	(define-struct alpha
	  (a b))

	(define {O alpha}
	  (make-alpha 1 2))

	(method-call a O 10)
	(method-call b O 20)
	(values (method-call a O)
		(method-call b O)))
    => 10 20)

;;; --------------------------------------------------------------------
;;; dot notation

  ;;Field accessors.
  ;;
  (check
      (internal-body

	(define-struct alpha
	  (a b))

	(define {O alpha}
	  (make-alpha 1 2))

	(values (.a O)
		(.b O)))
    => 1 2)

  ;;Field accessors and mutators.
  ;;
  (check
      (internal-body

	(define-struct alpha
	  (a b))

	(define {O alpha}
	  (make-alpha 1 2))

	(.a O 10)
	(.b O 20)
	(values (.a O)
		(.b O)))
    => 10 20)

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
