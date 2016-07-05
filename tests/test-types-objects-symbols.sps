;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the <symbol> type
;;;Date: Mon Oct 19, 2015
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
(program (test-types-symbol-objects)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) expander::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: tests for <symbol> objects\n")


(parametrise ((check-test-name		'predicate))

  (check-for-true	(is-a? 'ciao <symbol>))
  (check-for-true	(is-a? (gensym) <symbol>))
  (check-for-false	(is-a? 123 <symbol>))

  (check-for-true	(let (({O <symbol>} 'ciao))
			  (is-a? O <symbol>)))

  (check-for-true	(let (({O <top>} 'ciao))
			  (is-a? O <symbol>)))

  (check-for-false	(let (({O <top>} "ciao"))
			  (is-a? O <symbol>)))

  (void))


(parametrise ((check-test-name		'constructor))

  (check
      (new <symbol> "ciao")
    => 'ciao)

  (check
      (expander::type-signature.syntax-object (type-of (new <symbol> (read))))
    (=> expander::syntax=?)
    (list #'<symbol>))

  (void))


(parametrise ((check-test-name		'methods))

  (check
      (let (({O <symbol>} 'ciao))
	(.string O))
    => "ciao")

;;; --------------------------------------------------------------------

  (check
      (let (({O <symbol>} 'ciao))
	(fixnum? (.hash O)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let (({O <symbol>} 'ciao))
	(.bound? O))
    => #f)

  (check
      (let (({O <symbol>} (gensym)))
	(.value O 123)
	(.bound? O))
    => #t)

  (check
      (let (({O <symbol>} (gensym)))
	(.value O 123)
	(.value O))
    => 123)

;;; --------------------------------------------------------------------

  (.putprop 'ciao 'british 'hello)
  (.putprop 'ciao 'spanish 'hola)

  (check
      (let (({O <symbol>} 'ciao))
	(.getprop O 'british))
    => 'hello)

  (check
      (let (({O <symbol>} 'ciao))
	(.getprop O 'spanish))
    => 'hola)

  (check
      (let (({O <symbol>} 'ciao))
	(.property-list O))
    => '((british . hello)
	 (spanish . hola)))

  (.remprop 'ciao 'british)

  (check
      (let (({O <symbol>} 'ciao))
	(.getprop O 'british))
    => #f)

  (check
      (let (({O <symbol>} 'ciao))
	(.property-list O))
    => '((spanish . hola)))

  (void))


(parametrise ((check-test-name		'late-binding))

  (check
      (let (({O <symbol>} 'ciao))
	(method-call-late-binding 'string O))
    => "ciao")

;;; --------------------------------------------------------------------

  (check
      (let (({O <symbol>} 'ciao))
	(fixnum? (method-call-late-binding 'hash O)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let (({O <symbol>} 'ciao))
	(method-call-late-binding 'bound? O))
    => #f)

  (check
      (let (({O <symbol>} (gensym)))
	(method-call-late-binding 'value O 123)
	(method-call-late-binding 'bound? O))
    => #t)

  (check
      (let (({O <symbol>} (gensym)))
	(method-call-late-binding 'value O 123)
	(method-call-late-binding 'value O))
    => 123)

;;; --------------------------------------------------------------------

  (method-call-late-binding 'putprop 'ciao 'british 'hello)
  (method-call-late-binding 'putprop 'ciao 'spanish 'hola)

  (check
      (let (({O <symbol>} 'ciao))
	(method-call-late-binding 'getprop O 'british))
    => 'hello)

  (check
      (let (({O <symbol>} 'ciao))
	(method-call-late-binding 'getprop O 'spanish))
    => 'hola)

  (check
      (let (({O <symbol>} 'ciao))
	(method-call-late-binding 'property-list O))
    => '((spanish . hola)
	 (british . hello)))

  (method-call-late-binding 'remprop 'ciao 'british)

  (check
      (let (({O <symbol>} 'ciao))
	(method-call-late-binding 'getprop O 'british))
    => #f)

  (check
      (let (({O <symbol>} 'ciao))
	(method-call-late-binding 'property-list O))
    => '((spanish . hola)))

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
