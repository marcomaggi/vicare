;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for syntax utilities
;;;Date: Sat Oct  5, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (vicare language-extensions identifier-substitutions)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: identifier substitutions\n")


(parametrise ((check-test-name	'single))

  (define-syntax-rule (doit ?in-stx ?ou-stx)
    (check
	(let ((src-id	#'ciao)
	      (dst-stx	#'hello)
	      (stx	?in-stx))
	  (single-identifier-subst src-id dst-stx stx))
      (=> syntax=?)
      ?ou-stx))

;;; --------------------------------------------------------------------

  (doit	#'ciao #'hello)
  (doit #'(ciao) #'(hello))
  (doit #'#(ciao) #'#(hello))
  (doit #'(ciao salut ciao) #'(hello salut hello))
  (doit	#'#(ciao salut ciao) #'#(hello salut hello))
  (doit #'(ciao salut . ciao) #'(hello salut . hello))
  (doit	#'(ciao (salut (ciao))) #'(hello (salut (hello))))
  (doit #'(ciao ("salut" (ciao))) #'(hello ("salut" (hello))))

;;; --------------------------------------------------------------------
;;; quoting

  (doit #'(quote ciao) #'(quote ciao))
  (doit #'(123 (quote (ciao ciao)))
	#'(123 (quote (ciao ciao))))
  (doit #'(123 (quote (ciao ciao)) ciao)
	#'(123 (quote (ciao ciao)) hello))
  (doit #'(123 (quasiquote (quote (ciao ciao))) ciao)
	#'(123 (quasiquote (quote (ciao ciao))) hello))
  (doit #'(123 (quasiquote (unquote (ciao ciao))) ciao)
	#'(123 (quasiquote (unquote (hello hello))) hello))
  (doit #'(123 (quasiquote (ciao (unquote ciao))) ciao)
	#'(123 (quasiquote (ciao (unquote hello))) hello))

;;; --------------------------------------------------------------------
;;; syntaxing

  (doit #'(syntax ciao) #'(syntax ciao))
  (doit #'(123 (syntax (ciao ciao)))
	#'(123 (syntax (ciao ciao))))
  (doit #'(123 (syntax (ciao ciao)) ciao)
	#'(123 (syntax (ciao ciao)) hello))
  (doit #'(123 (quasisyntax (syntax (ciao ciao))) ciao)
	#'(123 (quasisyntax (syntax (ciao ciao))) hello))
  (doit #'(123 (quasisyntax (unsyntax (ciao ciao))) ciao)
	#'(123 (quasisyntax (unsyntax (hello hello))) hello))
  (doit #'(123 (quasisyntax (ciao (unsyntax ciao))) ciao)
	#'(123 (quasisyntax (ciao (unsyntax hello))) hello))

  #t)


;;;; done

(check-report)

;;; end of file
