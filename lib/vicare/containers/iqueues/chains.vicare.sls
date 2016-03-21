;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: iqueue interface for chains
;;;Date: Mon Aug 31, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare containers iqueues chains)
  (options typed-language)
  (export
    <iqueue-chain>
    make-iqueue-chain
    iqueue-chain?
    iqueue-chain-first-link
    iqueue-chain-last-link)
  (import (vicare)
    (vicare containers iqueues)
    (vicare containers chains))


;;;; type definition and core operations

(module (<iqueue-chain>
	 make-iqueue-chain iqueue-chain?
	 iqueue-chain-first-link
	 iqueue-chain-last-link)

  (define-record-type (<iqueue-chain> make-iqueue-chain iqueue-chain?)
    (nongenerative vicare:containers:<iqueue-chain>)
    (parent <iqueue>)
    (fields
     ;;Null or an instance of <chain-link> representing the first link in the chain.
     (mutable first-link iqueue-chain-first-link iqueue-chain-first-link-set!)
     ;;Null or an instance of <chain-link> representing the last link in the chain.
     (mutable last-link  iqueue-chain-last-link  iqueue-chain-last-link-set!))
    (protocol
     (lambda (make-iqueue)
       (lambda* ({C chain?})
	 ((make-iqueue iqueue-chain-empty? iqueue-chain-top
		       iqueue-chain-push! iqueue-chain-pop!)
	  (chain-front C)
	  (chain-rear  C)))))
    #| end of DEFINE-RECORD-TYPE |# )

  (define (iqueue-chain-top IS)
    (let ((first-lnk ($<iqueue-chain>-first-link IS)))
      (if (null? first-lnk)
	  ;;The chain is empty.
	  (assertion-violation __who__ "the container is empty" IS)
	($chain-link-ref first-lnk))))

  (define (iqueue-chain-push! IS obj)
    (let ((last-lnk  ($<iqueue-chain>-last-link IS))
	  (new-lnk   (make-chain-link obj)))
      (if (null? last-lnk)
	  ;;The chain is empty.
	  (begin
	    ($<iqueue-chain>-first-link-set! IS new-lnk)
	    ($<iqueue-chain>-last-link-set!  IS new-lnk))
	($<iqueue-chain>-last-link-set! IS ($chain-push-rear! last-lnk new-lnk)))))

  (define (iqueue-chain-pop! IS)
    (let ((first-lnk ($<iqueue-chain>-first-link IS)))
      (if (null? first-lnk)
	  ;;The chain is empty.
	  (assertion-violation __who__ "the container is empty" IS)
	(receive (old-first-link new-first-link)
	    ($chain-pop-front! ($<iqueue-chain>-first-link IS))
	  ($<iqueue-chain>-first-link-set! IS new-first-link)
	  (when (null? new-first-link)
	    ;;After popping the container is empty.
	    ($<iqueue-chain>-last-link-set! IS '()))
	  ($chain-link-ref old-first-link)))))

  (define (iqueue-chain-empty? IS)
    (null? ($<iqueue-chain>-first-link IS)))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
