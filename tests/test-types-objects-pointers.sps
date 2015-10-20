;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the <pointer> type
;;;Date: Tue Oct 20, 2015
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
(program (test-types-objects-pointers)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) xp.)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: tests for <pointer> objects\n")


;;;; helpers

(case-define %make-instance
  (()
   (integer->pointer 123))
  ((N)
   (integer->pointer N)))


(parametrise ((check-test-name	'predicate))

  (check-for-true	(is-a? (integer->pointer 123) <pointer>))
  (check-for-false	(is-a? 123 <pointer>))

  (check-for-true	(let (({O <pointer>} (integer->pointer 123)))
			  (is-a? O <pointer>)))

  (check-for-true	(let (({O <top>} (integer->pointer 123)))
			  (is-a? O <pointer>)))

  (check-for-false	(let (({O <top>} "ciao"))
			  (is-a? O <pointer>)))

  #t)


(parametrise ((check-test-name	'constructor))

  (check
      (pointer->integer (new <pointer> 123))
    => 123)

  (check
      (xp.type-signature-tags (type-of (new <pointer> (read))))
    (=> syntax=?)
    (list #'<pointer>))

  (void))


(parametrise ((check-test-name	'methods))

  (check-for-false
   (let (({O <pointer>} (%make-instance)))
     (.null? O)))

  (check-for-true
   (let (({O <pointer>} (%make-instance 0)))
     (.null? O)))

;;; --------------------------------------------------------------------

  (check
      (let (({O <pointer>} (%make-instance)))
	(.integer O))
    => 123)

  (check
      (let (({O <pointer>} (%make-instance)))
	(.= O (%make-instance)))
    => #t)

  (check
      (let (({O <pointer>} (%make-instance)))
	(.!= O (%make-instance)))
    => #f)

  (check
      (let (({O <pointer>} (%make-instance)))
	(.< O (%make-instance 222)))
    => #t)

  (check
      (let (({O <pointer>} (%make-instance)))
	(.> O (%make-instance 222)))
    => #f)

  (check
      (let (({O <pointer>} (%make-instance)))
	(.<= O (%make-instance 222)))
    => #t)

  (check
      (let (({O <pointer>} (%make-instance)))
	(.>= O (%make-instance 222)))
    => #f)

  (check
      (let (({O <pointer>} (%make-instance)))
	(.integer (.add O (- 999 123))))
    => 999)

  (check
      (let (({O <pointer>} (%make-instance))
	    ({P <pointer>} (%make-instance)))
	(.diff O P))
    => 0)

  (check
      (let (({O <pointer>} (%make-instance)))
	(.integer (.clone O)))
    => 123)

  (check
      (let (({O <pointer>} (%make-instance)))
	(.set-null! O)
	(.integer O))
    => 0)

  (check
      (let (({O <pointer>} (%make-instance)))
	(fixnum? (.hash O)))
    => #t)

  (void))


(parametrise ((check-test-name	'late-binding))

  (check-for-false
   (let (({O <pointer>} (%make-instance)))
     (method-call-late-binding 'null? O)))

  (check-for-true
   (let (({O <pointer>} (%make-instance 0)))
     (method-call-late-binding 'null? O)))

;;; --------------------------------------------------------------------


  (check
      (let (({O <pointer>} (%make-instance)))
	(method-call-late-binding 'integer O))
    => 123)

  (check
      (let (({O <pointer>} (%make-instance)))
	(method-call-late-binding '= O (%make-instance)))
    => #t)

  (check
      (let (({O <pointer>} (%make-instance)))
	(method-call-late-binding '!= O (%make-instance)))
    => #f)

  (check
      (let (({O <pointer>} (%make-instance)))
	(method-call-late-binding '< O (%make-instance 222)))
    => #t)

  (check
      (let (({O <pointer>} (%make-instance)))
	(method-call-late-binding '> O (%make-instance 222)))
    => #f)

  (check
      (let (({O <pointer>} (%make-instance)))
	(method-call-late-binding '<= O (%make-instance 222)))
    => #t)

  (check
      (let (({O <pointer>} (%make-instance)))
	(method-call-late-binding '>= O (%make-instance 222)))
    => #f)

  (check
      (let (({O <pointer>} (%make-instance)))
	(method-call-late-binding 'integer (method-call-late-binding 'add O (- 999 123))))
    => 999)

  (check
      (let (({O <pointer>} (%make-instance))
	    ({P <pointer>} (%make-instance)))
	(method-call-late-binding 'diff O P))
    => 0)

  (check
      (let (({O <pointer>} (%make-instance)))
	(method-call-late-binding 'integer (method-call-late-binding 'clone O)))
    => 123)

  (check
      (let (({O <pointer>} (%make-instance)))
	(method-call-late-binding 'set-null! O)
	(method-call-late-binding 'integer O))
    => 0)

  (check
      (let (({O <pointer>} (%make-instance)))
	(fixnum? (method-call-late-binding 'hash O)))
    => #t)


  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
