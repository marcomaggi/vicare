;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: built-in lists istack interface
;;;Date: Mon Aug 31, 2015
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
(library (vicare containers istacks lists)
  (export
    <istack-list>
    make-istack-list
    istack-list?
    istack-list-first-pair)
  (import (vicare)
    (vicare containers istacks))


;;;; type definition and core operations

(module (<istack-list> make-istack-list istack-list? istack-list-first-pair)

  (define-record-type (<istack-list> make-istack-list istack-list?)
    (nongenerative vicare:containers:<istack-list>)
    (parent <istack>)
    (fields (mutable first-pair istack-list-first-pair istack-list-first-pair-set!))
    (protocol
      (lambda (make-istack)
	(define (mk ell)
	  ((make-istack istack-list-empty? istack-list-top istack-list-push! istack-list-pop!) ell))
	(case-lambda*
	  (()
	   (mk '()))
	  (({L list?})
	   (mk L))))))

  (define (istack-list-top IL)
    (car ($<istack-list>-first-pair IL)))

  (define (istack-list-push! IL obj)
    ($<istack-list>-first-pair-set! IL (cons obj ($<istack-list>-first-pair IL))))

  (define (istack-list-pop! IL)
    (let ((ell ($<istack-list>-first-pair IL)))
      (begin0
	  (car ell)
	($<istack-list>-first-pair-set! IL (cdr ell)))))

  (define (istack-list-empty? IL)
    (null? ($<istack-list>-first-pair IL)))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
