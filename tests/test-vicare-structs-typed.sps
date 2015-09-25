;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for structs, typed language
;;;Date: Fri Sep 25, 2015
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


(parametrise ((check-test-name	'generic-rtd-syntax))

  (check
      (internal-body
	(define-struct alpha
	  (a b c))

	(eq? (struct-type-descriptor alpha)
	     (type-descriptor alpha)))
    => #t)

  #t)


(parametrise ((check-test-name	'generic-maker-syntax))

  (internal-body ;application syntax
    (define-struct alpha
      (a b c))

    (define-struct beta
      (a b))

    (check
	(let ((reco (new alpha 1 2 3)))
	  (alpha? reco))
      => #t)

    (check
	(let ((reco (new beta 1 2)))
	  (beta? reco))
      => #t)

    (void))

  #t)


(parametrise ((check-test-name	'generic-predicate-syntax))

  (internal-body

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

;;; --------------------------------------------------------------------

  (check
      (internal-body
	(define-struct duo
	  (one two))

	(is-a? (new duo 1 2) duo))
    => #t)

  #t)


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

  (check
      (internal-body

	(define-struct alpha
	  (a b))

	(define {O alpha}
	  (make-alpha 1 2))

	(values (.a O)
		(.b O)))
    => 1 2)

  (check
      (internal-body

	(define-struct alpha
	  (a b))

	(define {O alpha}
	  (make-alpha 1 2))

	(.a O 11)
	(.b O 22)
	(values (.a O)
		(.b O)))
    => 11 22)

  #t)


(parametrise ((check-test-name	'methods-late-binding))

;;; METHOD-CALL-LATE-BINDING, accessing fields

  ;;Accessing fields.
  ;;
  (check
      (internal-body

	(define-struct alpha
	  (a b))

	(define O
	  (make-alpha 1 2))

	(values (method-call-late-binding 'a O)
		(method-call-late-binding 'b O)))
    => 1 2)

;;; --------------------------------------------------------------------
;;; METHOD-CALL-LATE-BINDING, accessing and mutating fields

  ;;Accessing fields.
  ;;
  (check
      (internal-body

	(define-struct alpha
	  (a b))

	(define O
	  (new alpha 1 2))

	(method-call-late-binding 'a O 11)
	(method-call-late-binding 'b O 22)
	(values (method-call-late-binding 'a O)
		(method-call-late-binding 'b O)))
    => 11 22)

  (void))


;;;; done

(collect 'fullest)
(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
