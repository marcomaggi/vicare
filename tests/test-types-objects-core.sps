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
    (prefix (vicare expander) expander::)
    (only (vicare expander)
	  type-annotation=?
	  type-annotation-super-and-sub?
	  type-annotation-common-ancestor
	  type-annotation-ancestors
	  type-annotation-syntax
	  type-annotation-matching
	  type-signature-super-and-sub?
	  type-signature-common-ancestor
	  type-signature-matching
	  type-signature-union)
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
      (expander::type-signature.syntax-object (type-of (new <top> (read))))
    (=> expander::syntax=?)
    (list #'<top>))

;;; --------------------------------------------------------------------
;;; methods

  (check
      (fixnum? (.hash (unsafe-cast-signature (<top>) 123)))
    => #t)

  (check
      (fixnum? (method-call-late-binding 'hash #f (unsafe-cast-signature (<top>) 123)))
    => #t)

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
	(expander::type-signature.syntax-object (type-of (f 1))))
    (=> expander::syntax=?)
    #'<list>)

  (check
      (let (({f <procedure>} (unsafe-cast-signature (<procedure>) (lambda (x) x))))
	(expander::type-signature.syntax-object (type-of f)))
    (=> expander::syntax=?)
    (list #'<procedure>))

  #t)


(parametrise ((check-test-name	'hash-function))

  ;;Fixnums
  ;;
  (begin
    ;;Early binding.
    ;;
    (check
	(hash 123)
      => (fixnum-hash 123))

    ;;Late binding.
    ;;
    (check
	(let (({O <top>} 123))
	  (hash O))
      => (fixnum-hash 123))
    #| end of BEGIN |# )

  ;;Structs
  ;;
  (internal-body
    (define-struct duo
      (one two))

    (define O
      (make-duo 1 2))

    ;;Early binding.
    ;;
    (check
	(hash O)
      => (struct-hash O))

    ;;Late binding.
    ;;
    (check
	(let (({O <top>} O))
	  (hash O))
      => (struct-hash O))
    #| end of INTERNAL-BODY |# )

  ;;Records without hash function.
  ;;
  (internal-body
    (define-record-type duo
      (fields one two))

    (define O
      (make-duo 1 2))

    ;;Early binding.
    ;;
    (check
	(hash O)
      => (record-hash O))

    ;;Late binding.
    ;;
    (check
	(let (({O <top>} O))
	  (hash O))
      => (record-hash O))
    #| end of INTERNAL-BODY |# )

  ;;Records with hash function.
  ;;
  (internal-body
    (define-record-type duo
      (fields one two)
      (hash-function
	(lambda ()
	  (lambda (obj)
	    (fxadd1 (record-hash obj))))))

    (define O
      (make-duo 1 2))

    ;;Early binding.
    ;;
    (check
	(hash O)
      => (+ 1 (record-hash O)))

    ;;Late binding.
    ;;
    (check
	(let (({O <top>} O))
	  (hash O))
      => (+ 1 (record-hash O)))

    (void))

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
