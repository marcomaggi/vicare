;;;
;;;Part of: Vicare Scheme
;;;Contents: ideques interface for dynamic arrays
;;;Date: Mon Sep  7, 2015
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
(library (vicare containers ideques dynamic-arrays)
  (export
    <ideque-dynamic-array>
    make-ideque-dynamic-array
    ideque-dynamic-array?)
  (import (vicare)
    (vicare containers ideques)
    (vicare containers dynamic-arrays))


;;;; type definition and core operations

(module (<ideque-dynamic-array> make-ideque-dynamic-array ideque-dynamic-array?)

  (define-record-type (<ideque-dynamic-array> make-ideque-dynamic-array ideque-dynamic-array?)
    (nongenerative vicare:containers:<ideque-dynamic-array>)
    (parent <ideque>)
    (fields (immutable arry))
    (protocol
     (lambda (make-ideques)
       (lambda* ({A dynamic-array?})
	 ((make-ideques ideque-dynamic-array-empty?
			ideque-dynamic-array-front        ideque-dynamic-array-rear
			ideque-dynamic-array-push-front!  ideque-dynamic-array-push-rear!
			ideque-dynamic-array-pop-front!   ideque-dynamic-array-pop-rear!)

	  A)))))

  (define (ideque-dynamic-array-empty? ID)
    ($dynamic-array-empty? ($<ideque-dynamic-array>-arry ID)))

  (define (ideque-dynamic-array-front ID)
    ($dynamic-array-front ($<ideque-dynamic-array>-arry ID)))

  (define (ideque-dynamic-array-rear ID)
    ($dynamic-array-rear ($<ideque-dynamic-array>-arry ID)))

  (define (ideque-dynamic-array-push-front! ID obj)
    ($dynamic-array-push-front! ($<ideque-dynamic-array>-arry ID) obj))

  (define (ideque-dynamic-array-push-rear! ID obj)
    ($dynamic-array-push-rear! ($<ideque-dynamic-array>-arry ID) obj))

  (define (ideque-dynamic-array-pop-front! ID)
    ($dynamic-array-pop-front! ($<ideque-dynamic-array>-arry ID)))

  (define (ideque-dynamic-array-pop-rear! ID)
    ($dynamic-array-pop-rear! ($<ideque-dynamic-array>-arry ID)))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
