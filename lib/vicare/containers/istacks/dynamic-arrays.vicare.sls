;;;
;;;Part of: Vicare Scheme
;;;Contents: istack interface for dynamic arrays
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
(library (vicare containers istacks dynamic-arrays)
  (export
    <istack-dynamic-array>
    make-istack-dynamic-array
    istack-dynamic-array?)
  (import (vicare)
    (vicare containers istacks)
    (vicare containers dynamic-arrays))


;;;; type definition and core operations

(module (<istack-dynamic-array> make-istack-dynamic-array istack-dynamic-array?)

  (define-record-type (<istack-dynamic-array> make-istack-dynamic-array istack-dynamic-array?)
    (nongenerative vicare:containers:<istack-dynamic-array>)
    (parent <istack>)
    (fields (immutable arry))
    (protocol
     (lambda (make-istack)
       (lambda* ({A dynamic-array?})
	 ((make-istack istack-dynamic-array-empty? istack-dynamic-array-top
		       istack-dynamic-array-push! istack-dynamic-array-pop!)
	  A)))))

  (define (istack-dynamic-array-top IA)
    ($dynamic-array-front ($<istack-dynamic-array>-arry IA)))

  (define (istack-dynamic-array-push! IA obj)
    ($dynamic-array-push-front! ($<istack-dynamic-array>-arry IA) obj))

  (define (istack-dynamic-array-pop! IA)
    ($dynamic-array-pop-front! ($<istack-dynamic-array>-arry IA)))

  (define (istack-dynamic-array-empty? IA)
    ($dynamic-array-empty? ($<istack-dynamic-array>-arry IA)))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
