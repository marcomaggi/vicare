;;;
;;;Part of: Vicare Scheme
;;;Contents: ideque interface for chains
;;;Date: Mon Sep  7, 2015
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
(library (vicare containers ideques chains)
  (options typed-language)
  (export
    <ideque-chain>
    make-ideque-chain
    ideque-chain?
    ideque-chain-first-link
    ideque-chain-last-link)
  (import (vicare)
    (vicare containers ideques)
    (vicare containers chains))


;;;; type definition and core operations

(module (<ideque-chain>
	 make-ideque-chain ideque-chain?
	 ideque-chain-first-link
	 ideque-chain-last-link)

  (define-record-type (<ideque-chain> make-ideque-chain ideque-chain?)
    (nongenerative vicare:containers:<ideque-chain>)
    (parent <ideque>)
    (fields
     ;;Null or an instance of <chain-link> representing the first link in the chain.
     (mutable first-link ideque-chain-first-link ideque-chain-first-link-set!)
     ;;Null or an instance of <chain-link> representing the last link in the chain.
     (mutable last-link  ideque-chain-last-link  ideque-chain-last-link-set!))
    (protocol
      (lambda (make-ideque)
	(lambda* ({C chain?})
	  ((make-ideque ideque-chain-empty?
			ideque-chain-front        ideque-chain-rear
			ideque-chain-push-front!  ideque-chain-push-rear!
			ideque-chain-pop-front!   ideque-chain-pop-rear!)
	   (chain-front C)
	   (chain-rear  C)))))
    #| end of DEFINE-RECORD-TYPE |# )

  (define (ideque-chain-empty? IS)
    (null? ($<ideque-chain>-first-link IS)))

;;;

  (define (ideque-chain-front IS)
    (let ((first-lnk ($<ideque-chain>-first-link IS)))
      (if (null? first-lnk)
	  ;;The chain is empty.
	  (assertion-violation __who__ "the container is empty" IS)
	($chain-link-ref first-lnk))))

  (define (ideque-chain-rear IS)
    (let ((last-lnk ($<ideque-chain>-last-link IS)))
      (if (null? last-lnk)
	  ;;The chain is empty.
	  (assertion-violation __who__ "the container is empty" IS)
	($chain-link-ref last-lnk))))

;;;

  (define (ideque-chain-push-front! IS obj)
    (let ((first-lnk  ($<ideque-chain>-first-link IS))
	  (new-lnk    (make-chain-link obj)))
      (if (null? first-lnk)
	  ;;The chain is empty.
	  (begin
	    ($<ideque-chain>-first-link-set! IS new-lnk)
	    ($<ideque-chain>-last-link-set!  IS new-lnk))
	($<ideque-chain>-first-link-set! IS ($chain-push-front! first-lnk new-lnk)))))

  (define (ideque-chain-push-rear! IS obj)
    (let ((last-lnk  ($<ideque-chain>-last-link IS))
	  (new-lnk   (make-chain-link obj)))
      (if (null? last-lnk)
	  ;;The chain is empty.
	  (begin
	    ($<ideque-chain>-first-link-set! IS new-lnk)
	    ($<ideque-chain>-last-link-set!  IS new-lnk))
	($<ideque-chain>-last-link-set! IS ($chain-push-rear! last-lnk new-lnk)))))

;;;

  (define (ideque-chain-pop-front! IS)
    (let ((first-lnk ($<ideque-chain>-first-link IS)))
      (if (null? first-lnk)
	  ;;The chain is empty.
	  (assertion-violation __who__ "the container is empty" IS)
	(receive (old-first-link new-first-link)
	    ($chain-pop-front! first-lnk)
	  ($<ideque-chain>-first-link-set! IS new-first-link)
	  (when (null? new-first-link)
	    ;;After popping the container is empty.
	    ($<ideque-chain>-last-link-set! IS '()))
	  ($chain-link-ref old-first-link)))))

  (define (ideque-chain-pop-rear! IS)
    (let ((last-lnk ($<ideque-chain>-last-link IS)))
      (if (null? last-lnk)
	  ;;The chain is empty.
	  (assertion-violation __who__ "the container is empty" IS)
	(receive (old-last-link new-last-link)
	    ($chain-pop-rear! last-lnk)
	  ($<ideque-chain>-last-link-set! IS new-last-link)
	  (when (null? new-last-link)
	    ;;After popping the container is empty.
	    ($<ideque-chain>-first-link-set! IS '()))
	  ($chain-link-ref old-last-link)))))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
