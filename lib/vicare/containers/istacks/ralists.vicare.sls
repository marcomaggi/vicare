;;;
;;;Part of: Vicare Scheme
;;;Contents: istack interface for random-access lists
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
(library (vicare containers istacks ralists)
  (export
    <istack-ralist>
    make-istack-ralist
    istack-ralist?)
  (import (vicare)
    (vicare containers istacks)
    (prefix (vicare containers ralists) ra))


;;;; type definitions and core operations

(module (<istack-ralist> make-istack-ralist istack-ralist?)

  (define-record-type (<istack-ralist> make-istack-ralist istack-ralist?)
    (nongenerative vicare:containers:<istack-ralist>)
    (parent <istack>)
    (fields (mutable ell))
    (protocol
     (lambda (make-istack)
       (lambda* ({L ralist?})
	 ((make-istack istack-ralist-top istack-ralist-push! istack-ralist-pop! istack-ralist-empty?)
	  L)))))

  (define (istack-ralist-top IL)
    (racar ($<istack-ralist>-ell IL)))

  (define (istack-ralist-push! IL obj)
    ($<istack-ralist>-ell-set! IL (racons obj ($<istack-ralist>-ell IL))))

  (define (istack-ralist-pop! IL)
    (let ((ell ($<istack-ralist>-ell IL)))
      (begin0
	  (racar ell)
	($<istack-ralist>-ell-set! IL (racdr ell)))))

  (define (istack-ralist-empty? IL)
    (null? ($<istack-ralist>-ell IL)))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
