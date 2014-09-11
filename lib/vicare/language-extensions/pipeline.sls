;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: pipeline syntax
;;;Date: Tue Sep  9, 2014
;;;
;;;Abstract
;;;
;;;	The original  version of this macro  was posted on comp.lang.scheme  by Marko
;;;	Rauhamaa, thread "Pipeline macro" on Tue, 09 Sep 2014 01:00:47 +0300.
;;;
;;;Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(library (vicare language-extensions pipeline)
  (export pipeline =>)
  (import (rnrs (6)))


(define-syntax pipeline
  (lambda (stx)
    (define (main stx)
      (syntax-case stx ()
	((_ . ?clause*)
	 (generate-nested-exprs #'?clause*))))

    (define (generate-nested-exprs clause*)
      (syntax-case clause* (=>)
	((=> . ?stuff)
	 (synner "missing producer expression" clause*))

	((?producer-expr => ?formals)
	 (synner "receiver variables specified without consumer expression" #'?formals))

	((?producer-expr => ?formals . ?consumer-expr*)
	 (if (proper-or-improper-identifiers? #'?formals)
	     #`(call-with-values
		   (lambda () ?producer-expr)
		 (lambda ?formals #,(generate-nested-exprs #'?consumer-expr*)))
	   (synner "expected proper or improper list of identifiers as receiver formals"
		   #'?formals)))

	((?final-expr)
	 #'?final-expr)
	))

    (define (proper-or-improper-identifiers? formals)
      (syntax-case formals ()
	(() #t)
	(?rest
	 (identifier? #'?rest)
	 #t)
	((?car . ?cdr)
	 (identifier? #'?car)
	 (proper-or-improper-identifiers? #'?cdr))
	(_  #f)))

    (define (synner message subform)
      (syntax-violation 'pipeline message stx subform))

    (main stx)))


;;;; done

)

;;; end of file
;; Local Variables:
;; End:
