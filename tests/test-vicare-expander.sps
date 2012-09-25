;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for the expander
;;;Date: Tue Sep 25, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(import (vicare)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare expander\n")


(parametrise ((check-test-name	'import))

  (check	;import separately a named module and a library, library
		;first
      (let ()
	(module ciao
	  (hello salut)
	  (define (hello) 'hello)
	  (define (salut) 'salut))
	(import (vicare syntactic-extensions))
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
	(import (vicare syntactic-extensions))
	(list (hello) (salut)))
    => '(hello salut))

  (check	;import both a named module and a library, library first
      (let ()
	(module ciao
	  (hello salut)
	  (define (hello) 'hello)
	  (define (salut) 'salut))
	(import (vicare syntactic-extensions)
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
	  (vicare syntactic-extensions))
	(list (hello) (salut)))
    => '(hello salut))

  #;(check	;import a named module with some name mangling
      (let ()
	(module ciao
	  (hello salut)
	  (define (hello) 'hello)
	  (define (salut) 'salut))
	(import (prefix ciao ciao.))
	(list (ciao.hello) (ciao.salut)))
    => '(hello salut))

  #t)


;;;; done

(check-report)

;;; end of file
