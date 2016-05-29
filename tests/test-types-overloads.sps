;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for overloaded functions
;;;Date: Sun May 29, 2016
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
(program (test-types-overloads)
  (options typed-language)
  (import (vicare)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: tests for overloaded functions\n")


(parametrise ((check-test-name	'base))

  (check
      (internal-body
	(define/overload (fun {O <fixnum>})
	  (list 'fixnum O))

	(define/overload (fun {O <string>})
	  (list 'string O))

	(define/overload (fun {A <vector>} {B <vector>})
	  (list 'vectors (vector-append A B)))

	(values (fun 123)
		(fun "ciao")
		(fun '#(1) '#(2))))
    => '(fixnum 123) '(string "ciao") '(vectors #(1 2)))

  #t)


(parametrise ((check-test-name	'records))

;;; no parent

  (check
      (internal-body

	(define-record-type alpha
	  (fields (mutable a) (mutable b))
	  (method/overload (get-a O)
	    (alpha-a O))
	  (method/overload (get-b O)
	    (alpha-b O)))

	(define {O alpha}
	  (make-alpha 1 2))

	(values (method-call get-a O)
		(method-call get-b O))
	(values 1 2))
    => 1 2)

  (check
      (internal-body

	(define-record-type alpha
	  (fields (mutable a) (mutable b))
	  (method/overload (get-a O)
	    (alpha-a O))
	  (method/overload (get-b O)
	    (alpha-b O))
	  (method/overload (set-a O v)
	    (alpha-a-set! O v))
	  (method/overload (set-b O v)
	    (alpha-b-set! O v)))

	(define {O alpha}
	  (make-alpha 1 2))

	(method-call set-a O 10)
	(method-call set-b O 20)
	(values (method-call get-a O)
		(method-call get-b O)))
    => 10 20)

;;; --------------------------------------------------------------------
;;; calling parent's method/overloads

  ;;Record-type with parent.
  ;;
  (check
      (internal-body

	(define-record-type duo
	  (fields one two)
	  (method/overload (sum-them O)
	    (+ (duo-one O)
	       (duo-two O))))

	(define-record-type trio
	  (parent duo)
	  (fields three)
	  (method/overload (mul-them O)
	    (* (duo-one O)
	       (duo-two O)
	       (trio-three O))))

	(define {O trio}
	  (make-trio 3 5 7))

	(values (method-call sum-them O)
		(method-call mul-them O)))
    => (+ 3 5) (* 3 5 7))

  ;;Record-type with parent and grandparent.
  ;;
  (check
      (internal-body

	(define-record-type duo
	  (fields one two)
	  (method/overload (sum-them O)
	    (+ (duo-one O)
	       (duo-two O))))

	(define-record-type trio
	  (parent duo)
	  (fields three)
	  (method/overload (mul-them O)
	    (* (duo-one O)
	       (duo-two O)
	       (trio-three O))))

	(define-record-type quater
	  (parent trio)
	  (fields four)
	  (method/overload (list-them O)
	    (list (duo-one O)
		  (duo-two O)
		  (trio-three O)
		  (quater-four O))))

	(define {O quater}
	  (make-quater 3 5 7 11))

	(values (method-call sum-them O)
		(method-call mul-them O)
		(method-call list-them O)))
    => (+ 3 5) (* 3 5 7) (list 3 5 7 11))

;;; --------------------------------------------------------------------
;;; dot notation

  (check
      (internal-body

	(define-record-type alpha
	  (fields (mutable a) (mutable b))
	  (method/overload (get-a O)
	    (alpha-a O))
	  (method/overload (get-b O)
	    (alpha-b O)))

	(define {O alpha}
	  (make-alpha 1 2))

	(values (.get-a O)
		(.get-b O)))
    => 1 2)

  (check
      (internal-body

	(define-record-type alpha
	  (fields (mutable a) (mutable b))
	  (method/overload (get-a O)
	    (alpha-a O))
	  (method/overload (get-b O)
	    (alpha-b O))
	  (method/overload (set-a O v)
	    (alpha-a-set! O v))
	  (method/overload (set-b O v)
	    (alpha-b-set! O v)))

	(define {O alpha}
	  (make-alpha 1 2))

	(.set-a O 10)
	(.set-b O 20)
	(values (.get-a O)
		(.get-b O)))
    => 10 20)

;;; --------------------------------------------------------------------
;;; actual overloadgin with multiple implementations

  (check
      (internal-body

	(define-record-type alpha
	  (fields a b)
	  (method/overload (doit {O alpha} {A <fixnum>})
	    (list (.a O) (.b O) 'fixnum A))
	  (method/overload (doit {O alpha} {A <symbol>})
	    (list (.a O) (.b O) 'symbol A))
	  (method/overload (doit {O alpha} {A <number>} {B <number>})
	    (list (.a O) (.b O) 'numbers A B)))

	(define {O alpha}
	  (make-alpha 1 2))

	(values (.doit O 123)
		(.doit O 'ciao)
		(.doit O 3 4)))
    => '(1 2 fixnum 123) '(1 2 symbol ciao) '(1 2 numbers 3 4))

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
