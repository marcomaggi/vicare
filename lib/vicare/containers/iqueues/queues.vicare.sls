;;;
;;;Part of: Vicare Scheme
;;;Contents: iqueue interface for queues
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
(library (vicare containers iqueues queues)
  (export
    <iqueue-queue>
    make-iqueue-queue
    iqueue-queue?)
  (import (vicare)
    (vicare containers iqueues)
    (vicare containers queues))


;;;; type definition and core operations

(module (<iqueue-queue> make-iqueue-queue iqueue-queue?)

  (define-record-type (<iqueue-queue> make-iqueue-queue iqueue-queue?)
    (nongenerative vicare:containers:<iqueue-queue>)
    (parent <iqueue>)
    (fields (immutable queue))
    (protocol
     (lambda (make-iqueue)
       (lambda* ({D queue?})
	 ((make-iqueue iqueue-queue-empty? iqueue-queue-top
		       iqueue-queue-push!  iqueue-queue-pop!)
	  D)))))

  (define (iqueue-queue-top IS)
    ($queue-front ($<iqueue-queue>-queue IS)))

  (define (iqueue-queue-push! IS obj)
    ($queue-push! ($<iqueue-queue>-queue IS) obj))

  (define (iqueue-queue-pop! IS)
    ($queue-pop! ($<iqueue-queue>-queue IS)))

  (define (iqueue-queue-empty? IS)
    ($queue-empty? ($<iqueue-queue>-queue IS)))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
