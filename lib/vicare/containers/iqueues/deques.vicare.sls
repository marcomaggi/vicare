;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: iqueue interface for deques
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
(library (vicare containers iqueues deques)
  (export
    <iqueue-deque>
    make-iqueue-deque
    iqueue-deque?)
  (import (vicare)
    (vicare containers iqueues)
    (vicare containers deques))


;;;; type definition and core operations

(module (<iqueue-deque> make-iqueue-deque iqueue-deque?)

  (define-record-type (<iqueue-deque> make-iqueue-deque iqueue-deque?)
    (nongenerative vicare:containers:<iqueue-deque>)
    (parent <iqueue>)
    (fields (immutable deque))
    (protocol
     (lambda (make-iqueue)
       (lambda* ({D deque?})
	 ((make-iqueue iqueue-deque-empty? iqueue-deque-top
		       iqueue-deque-push! iqueue-deque-pop!)
	  D)))))

  (define (iqueue-deque-top ID)
    ($deque-front ($<iqueue-deque>-deque ID)))

  (define (iqueue-deque-push! ID obj)
    ($deque-push-rear! ($<iqueue-deque>-deque ID) obj))

  (define (iqueue-deque-pop! ID)
    ($deque-pop-front! ($<iqueue-deque>-deque ID)))

  (define (iqueue-deque-empty? ID)
    ($deque-empty? ($<iqueue-deque>-deque ID)))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
