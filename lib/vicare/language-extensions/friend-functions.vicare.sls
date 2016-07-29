;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: friend functions definition
;;;Date: Fri Jul 29, 2016
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
(library (vicare language-extensions friend-functions (0 4 2016 07 29))
  (options typed-language)
  (export define/friend)
  (import (vicare)
    (only (psyntax system $all)
	  typed-variable-with-private-access!))

  (define-syntax (define/friend stx)
    (syntax-case stx (brace)
      ((_ (?who (brace ?subject ?ann) . ?formals) ?body0 ?body ...)
       #'(define/checked (?who (brace ?subject ?ann) . ?formals)
	   (typed-variable-with-private-access! ?subject)
	   ?body0 ?body ...))
      ))

  #| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
