;;;
;;;Part of: Vicare Scheme
;;;Contents: istack interface for deques
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
(library (vicare containers istacks deques)
  (export
    <istack-deque>
    make-istack-deque
    istack-deque?)
  (import (vicare)
    (vicare containers istacks)
    (vicare containers deques))


;;;; type definition and core operations

(module (<istack-deque> make-istack-deque istack-deque?)

  (define-record-type (<istack-deque> make-istack-deque istack-deque?)
    (nongenerative vicare:containers:<istack-deque>)
    (parent <istack>)
    (fields (immutable deque))
    (protocol
     (lambda (make-istack)
       (lambda* ({D deque?})
	 ((make-istack istack-deque-top istack-deque-push! istack-deque-pop! istack-deque-empty?)
	  D)))))

  (define (istack-deque-top ID)
    ($deque-front ($<istack-deque>-deque ID)))

  (define (istack-deque-push! ID obj)
    ($deque-push-front! ($<istack-deque>-deque ID) obj))

  (define (istack-deque-pop! ID)
    ($deque-pop-front! ($<istack-deque>-deque ID)))

  (define (istack-deque-empty? ID)
    ($deque-empty? ($<istack-deque>-deque ID)))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
