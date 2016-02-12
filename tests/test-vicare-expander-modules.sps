;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the expander
;;;Date: Fri Feb 12, 2016
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012-2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(program (test-vicare-expander-modules)
  (import (vicare)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare's expander: modules interface\n")


(parametrise ((check-test-name	'basic-import))

  (check	;import separately a named module and a library, library
		;first
      (let ()
	(module ciao
	  (hello salut)
	  (define (hello) 'hello)
	  (define (salut) 'salut))
	(import (vicare language-extensions syntaxes))
	(import ciao)
	(list (hello) (salut)))
    => '(hello salut))

  (check	;import separately a named  module and a library, module
		;first
      (let ()
	(module ciao
	  (hello salut)
	  (define (hello) 'hello)
	  (define (salut) 'salut))
	(import ciao)
	(import (vicare language-extensions syntaxes))
	(list (hello) (salut)))
    => '(hello salut))

  (check	;import both a named module and a library, library first
      (let ()
	(module ciao
	  (hello salut)
	  (define (hello) 'hello)
	  (define (salut) 'salut))
	(import (vicare language-extensions syntaxes)
	  ciao)
	(list (hello) (salut)))
    => '(hello salut))

  (check	;import both a named module and a library, module first
      (let ()
	(module ciao
	  (hello salut)
	  (define (hello) 'hello)
	  (define (salut) 'salut))
	(import ciao
	  (vicare language-extensions syntaxes))
	(list (hello) (salut)))
    => '(hello salut))

  #t)


(parametrise ((check-test-name	'extended-import))

  ;;Import a named module with prefix.
  ;;
  (check
      (let ()
	(module CIAO
	  (hello salut)
	  (define (hello) 'hello)
	  (define (salut) 'salut))
	(import (prefix CIAO ciao::))
	(list (ciao::hello) (ciao::salut)))
    => '(hello salut))

  ;;Import a named module with prefix then deprefix.
  ;;
  (check
      (let ()
	(module CIAO
	  (hello salut)
	  (define (hello) 'hello)
	  (define (salut) 'salut))
	(import (deprefix (prefix CIAO hey-ciao::) hey-))
	(list (ciao::hello) (ciao::salut)))
    => '(hello salut))

  ;;Import a named module with renaming.
  ;;
  (check
      (let ()
	(module CIAO
	  (hello salut)
	  (define (hello) 'hello)
	  (define (salut) 'salut))
	(import (rename CIAO
			(hello	Hello)
			(salut	Salut)))
	(list (Hello) (Salut)))
    => '(hello salut))

  ;;Import a named module with renaming and prefixing.
  ;;
  (check
      (let ()
	(module CIAO
	  (hello salut)
	  (define (hello) 'hello)
	  (define (salut) 'salut))
	(import (prefix (rename CIAO
				(hello	Hello)
				(salut	Salut))
			ciao::))
	(list (ciao::Hello) (ciao::Salut)))
    => '(hello salut))

  ;;Import a named module with double renaming.
  ;;
  (check
      (let ()
	(module CIAO
	  (hello salut)
	  (define (hello) 'hello)
	  (define (salut) 'salut))
	(import (rename (rename CIAO
				(hello	Hello)
				(salut	Salut))
			(Hello	my-hello)
			(Salut	my-salut)))
	(list (my-hello) (my-salut)))
    => '(hello salut))

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; eval: (put 'typ.set-identifier-tag-type-spec!	'scheme-indent-function 1)
;; eval: (put 'catch-syntax-violation			'scheme-indent-function 1)
;; eval: (put 'case-identifiers				'scheme-indent-function 1)
;; End:
