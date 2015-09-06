;;;
;;;Part of: Vicare Scheme
;;;Contents: istack interface for immutable lists
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
(library (vicare containers istacks ilists)
  (export
    <istack-ilist>
    make-istack-ilist
    istack-ilist?
    istack-ilist-first-pair)
  (import (vicare)
    (vicare containers istacks)
    (vicare containers ilists))


;;;; type definitions and core operations

(module (<istack-ilist> make-istack-ilist istack-ilist? istack-ilist-first-pair)

  (define-record-type (<istack-ilist> make-istack-ilist istack-ilist?)
    (nongenerative vicare:containers:<istack-ilist>)
    (parent <istack>)
    (fields (mutable first-pair istack-ilist-first-pair istack-ilist-first-pair-set!))
    (protocol
     (lambda (make-istack)
       (define (mk ell)
	 ((make-istack istack-ilist-empty? istack-ilist-top
		       istack-ilist-push! istack-ilist-pop!)
	  ell))
       (case-lambda*
	 (()
	  (mk '()))
	 (({L ilist?})
	  (mk L))))))

  (define (istack-ilist-top IL)
    (icar ($<istack-ilist>-first-pair IL)))

  (define (istack-ilist-push! IL obj)
    ($<istack-ilist>-first-pair-set! IL (ipair obj ($<istack-ilist>-first-pair IL))))

  (define (istack-ilist-pop! IL)
    (let ((ell ($<istack-ilist>-first-pair IL)))
      (begin0
	  (icar ell)
	($<istack-ilist>-first-pair-set! IL (icdr ell)))))

  (define (istack-ilist-empty? IL)
    (null? ($<istack-ilist>-first-pair IL)))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
