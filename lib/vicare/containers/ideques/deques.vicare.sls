;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: ideques interface for deques
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
(library (vicare containers ideques deques)
  (export
    <ideque-deque>
    make-ideque-deque
    ideque-deque?)
  (import (vicare)
    (vicare containers ideques)
    (vicare containers deques))


;;;; type definition and core operations

(module (<ideque-deque> make-ideque-deque ideque-deque?)

  (define-record-type (<ideque-deque> make-ideque-deque ideque-deque?)
    (nongenerative vicare:containers:<ideque-deque>)
    (parent <ideque>)
    (fields (immutable deque))
    (protocol
     (lambda (make-ideques)
       (lambda* ({D deque?})
	 ((make-ideques ideque-deque-empty?
			ideque-deque-front        ideque-deque-rear
			ideque-deque-push-front!  ideque-deque-push-rear!
			ideque-deque-pop-front!   ideque-deque-pop-rear!)
	  D)))))

  (define (ideque-deque-empty? ID)
    ($deque-empty? ($<ideque-deque>-deque ID)))

  (define (ideque-deque-front ID)
    ($deque-front ($<ideque-deque>-deque ID)))

  (define (ideque-deque-rear ID)
    ($deque-rear ($<ideque-deque>-deque ID)))

  (define (ideque-deque-push-front! ID obj)
    ($deque-push-front! ($<ideque-deque>-deque ID) obj))

  (define (ideque-deque-push-rear! ID obj)
    ($deque-push-rear! ($<ideque-deque>-deque ID) obj))

  (define (ideque-deque-pop-front! ID)
    ($deque-pop-front! ($<ideque-deque>-deque ID)))

  (define (ideque-deque-pop-rear! ID)
    ($deque-pop-rear! ($<ideque-deque>-deque ID)))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
