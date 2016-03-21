;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: istack interface for chains
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
(library (vicare containers istacks chains)
  (export
    <istack-chain>
    make-istack-chain
    istack-chain?
    istack-chain-first-link)
  (import (vicare)
    (vicare containers istacks)
    (vicare containers chains))


;;;; type definitions and core operations

(module (<istack-chain> make-istack-chain istack-chain? istack-chain-first-link)

  (define-record-type (<istack-chain> make-istack-chain istack-chain?)
    (nongenerative vicare:containers:<istack-chain>)
    (parent <istack>)
    (fields (mutable first-link istack-chain-first-link istack-chain-first-link-set!))
		;Null or an  instance of <chain-link> representing the  first link in
		;the chain.
    (protocol
     (lambda (make-istack)
       (lambda* ({C chain?})
	 ((make-istack istack-chain-empty? istack-chain-top
		       istack-chain-push! istack-chain-pop!)
	  C)))))

  (define (istack-chain-top IS)
    ($chain-link-ref ($chain-front ($<istack-chain>-first-link IS))))

  (define (istack-chain-push! IS obj)
    ($<istack-chain>-first-link-set! IS ($chain-push-front! ($<istack-chain>-first-link IS)
							    (make-chain-link obj))))

  (define (istack-chain-pop! IS)
    (receive (top-link new-first-link)
	($chain-pop-front! ($<istack-chain>-first-link IS))
      ($<istack-chain>-first-link-set! IS new-first-link)
      ($chain-link-ref top-link)))

  (define (istack-chain-empty? IS)
    (null? ($<istack-chain>-first-link IS)))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
