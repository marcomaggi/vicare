;;;
;;;Part of: Vicare Scheme
;;;Contents: generic interface for deque-like structures
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
(library (vicare containers ideques)
  (export
    <ideque>		ideque?
    ideque-empty?
    ideque-front	ideque-rear
    ideque-push-front!	ideque-push-rear!
    ideque-pop-front!	ideque-pop-rear!)
  (import (vicare))


;;;; type definition and core operations

(define-record-type (<ideque> make-ideque ideque?)
  (nongenerative vicare:containers:<ideque>)
  (fields (immutable empty?)
	  (immutable front)
	  (immutable rear)
	  (immutable push-front!)
	  (immutable push-rear!)
	  (immutable pop-front!)
	  (immutable pop-rear!))
  (protocol
   (lambda (make-record)
     (lambda (empty? front rear push-front push-rear pop-front pop-rear)
       (make-record empty? front rear push-front push-rear pop-front pop-rear))))
  #| end of DEFINE-RECORD-TYPE|# )

(define* (ideque-front {S ideque?})
  (($<ideque>-front S) S))

(define* (ideque-rear {S ideque?})
  (($<ideque>-rear S) S))

(define* (ideque-push-front! {S ideque?} obj)
  (($<ideque>-push-front! S) S obj))

(define* (ideque-push-rear! {S ideque?} obj)
  (($<ideque>-push-rear! S) S obj))

(define* (ideque-pop-front! {S ideque?})
  (($<ideque>-pop-front! S) S))

(define* (ideque-pop-rear! {S ideque?})
  (($<ideque>-pop-rear! S) S))

(define* (ideque-empty? {S ideque?})
  (($<ideque>-empty? S) S))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
