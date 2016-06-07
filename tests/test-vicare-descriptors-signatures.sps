;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for object-type descriptors signatures
;;;Date: Fri Jun  3, 2016
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
(program (test-vicare-descriptors-signatures)
  (options typed-language)
  (import (except (vicare)
		  <list> <null> <nelist>)
    (vicare system type-descriptors)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: object-type descriptor signatures\n")


(parametrise ((check-test-name	'matching-super-and-sub))

  (define-syntax doit-true
    (syntax-rules ()
      ((_ ?super ?sub)
       (check-for-true
	(descriptors-signature.matching-super-and-sub? ?super ?sub)))
      ))

  (define-syntax doit-false
    (syntax-rules ()
      ((_ ?super ?sub)
       (check-for-true
	(descriptors-signature.matching-super-and-sub? ?super ?sub)))
      ))

  (define-syntax (DS stx)
    (define (%parse-ids stx)
      (syntax-case stx ()
	((?id . ?ids)
	 (identifier? #'?id)
	 #`(cons (type-descriptor ?id) #,(%parse-ids #'?ids)))
	;; ((?id . ?ids)
	;;  #`(cons (type-descriptor ?id) #,(%parse-ids #'?ids)))
	(()
	 #'(quote ()))
	(?id
	 (identifier? #'?id)
	 #'(type-descriptor ?id))
	))
    ;; (define (%parse-compound stx)
    ;;   (syntax-case stx (pair pair-of list list-of vector vector-of ancestor-of enumeration)
    ;; 	(())))
    (syntax-case stx ()
      ((_ . ?ids)
       #`(new <descriptors-signature> #,(%parse-ids #'?ids)))
      ))

;;; --------------------------------------------------------------------

  (doit-true (DS <fixnum> <fixnum>)
	     (DS <fixnum> <fixnum>))

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
