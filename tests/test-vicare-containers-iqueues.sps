;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for iqueues
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


#!r6rs
(import (vicare)
  (vicare containers queues)
  (vicare containers deques)
  (vicare containers dynamic-arrays)
  (vicare containers chains)
  (vicare containers iqueues)
  (vicare containers iqueues queues)
  (vicare containers iqueues deques)
  (vicare containers iqueues dynamic-arrays)
  (vicare containers iqueues chains)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: tests for iqueues\n")


(parametrise ((check-test-name	'queues))

  (check
      (let* ((D	(queue 0 1 2 3))
	     (S	(make-iqueue-queue D)))
	(iqueue-empty? S))
    => #f)

  (check
      (let* ((D	(queue))
	     (S	(make-iqueue-queue D)))
	(iqueue-empty? S))
    => #t)

  (check
      (let* ((D	(queue))
	     (S	(make-iqueue-queue D)))
	(iqueue-push! S 0)
	(let* ((rv1 (iqueue-top  S))
	       (rv2 (iqueue-pop! S)))
	  (values rv1 rv2 (iqueue-empty? S))))
    => 0 0 #t)

  (check
      (let* ((D	(queue))
	     (S	(make-iqueue-queue D)))
	(iqueue-push! S 0)
	(iqueue-push! S 1)
	(iqueue-push! S 2)
	(let* ((rv1 (iqueue-top  S))
	       (rv2 (iqueue-pop! S))
	       (rv3 (iqueue-top  S))
	       (rv4 (iqueue-pop! S))
	       (rv5 (iqueue-top  S))
	       (rv6 (iqueue-pop! S)))
	  (values rv1 rv2  rv3 rv4  rv5 rv6
		  (iqueue-empty? S))))
    => 0 0 1 1 2 2 #t)

;;; --------------------------------------------------------------------
;;; type syntaxes

  (check
      (let* ((D	(queue 0 1 2 3))
	     (S	(new <iqueue-queue> D)))
	(iqueue-empty? S))
    => #f)

  (check
      (let* ((D	(new <queue>))
	     (S	(new <iqueue-queue> D)))
	(iqueue-empty? S))
    => #t)

  (check
      (let* ((D	(new <queue>))
	     (S	(new <iqueue-queue> D)))
	(iqueue-push! S 0)
	(let* ((rv1 (iqueue-top  S))
	       (rv2 (iqueue-pop! S)))
	  (values rv1 rv2 (iqueue-empty? S))))
    => 0 0 #t)

  (check
      (let* ((D	(new <queue>))
	     (S	(new <iqueue-queue> D)))
	(iqueue-push! S 0)
	(iqueue-push! S 1)
	(iqueue-push! S 2)
	(let* ((rv1 (iqueue-top  S))
	       (rv2 (iqueue-pop! S))
	       (rv3 (iqueue-top  S))
	       (rv4 (iqueue-pop! S))
	       (rv5 (iqueue-top  S))
	       (rv6 (iqueue-pop! S)))
	  (values rv1 rv2  rv3 rv4  rv5 rv6
		  (iqueue-empty? S))))
    => 0 0 1 1 2 2 #t)

  #t)


(parametrise ((check-test-name	'deques))

  (check
      (let* ((D	(deque 0 1 2 3))
	     (S	(make-iqueue-deque D)))
	(iqueue-empty? S))
    => #f)

  (check
      (let* ((D	(deque))
	     (S	(make-iqueue-deque D)))
	(iqueue-empty? S))
    => #t)

  (check
      (let* ((D	(deque))
	     (S	(make-iqueue-deque D)))
	(iqueue-push! S 0)
	(let* ((rv1 (iqueue-top  S))
	       (rv2 (iqueue-pop! S)))
	  (values rv1 rv2 (iqueue-empty? S))))
    => 0 0 #t)

  (check
      (let* ((D	(deque))
	     (S	(make-iqueue-deque D)))
	(iqueue-push! S 0)
	(iqueue-push! S 1)
	(iqueue-push! S 2)
	(let* ((rv1 (iqueue-top  S))
	       (rv2 (iqueue-pop! S))
	       (rv3 (iqueue-top  S))
	       (rv4 (iqueue-pop! S))
	       (rv5 (iqueue-top  S))
	       (rv6 (iqueue-pop! S)))
	  (values rv1 rv2  rv3 rv4  rv5 rv6
		  (iqueue-empty? S))))
    => 0 0 1 1 2 2 #t)

;;; --------------------------------------------------------------------

  #t)


(parametrise ((check-test-name	'dynamic-arrays))

  (check
      (let* ((D	(dynamic-array 0 1 2 3))
	     (S	(make-iqueue-dynamic-array D)))
	(iqueue-empty? S))
    => #f)

  (check
      (let* ((D	(dynamic-array))
	     (S	(make-iqueue-dynamic-array D)))
	(iqueue-empty? S))
    => #t)

  (check
      (let* ((D	(dynamic-array))
	     (S	(make-iqueue-dynamic-array D)))
	(iqueue-push! S 0)
	(let* ((rv1 (iqueue-top  S))
	       (rv2 (iqueue-pop! S)))
	  (values rv1 rv2 (iqueue-empty? S))))
    => 0 0 #t)

  (check
      (let* ((D	(dynamic-array))
	     (S	(make-iqueue-dynamic-array D)))
	(iqueue-push! S 0)
	(iqueue-push! S 1)
	(iqueue-push! S 2)
	(let* ((rv1 (iqueue-top  S))
	       (rv2 (iqueue-pop! S))
	       (rv3 (iqueue-top  S))
	       (rv4 (iqueue-pop! S))
	       (rv5 (iqueue-top  S))
	       (rv6 (iqueue-pop! S)))
	  (values rv1 rv2  rv3 rv4  rv5 rv6
		  (iqueue-empty? S))))
    => 0 0 1 1 2 2 #t)

;;; --------------------------------------------------------------------

  #t)


(parametrise ((check-test-name	'chains))

  (check
      (let* ((D	(chain 0 1 2 3))
	     (S	(make-iqueue-chain D)))
	(iqueue-empty? S))
    => #f)

  (check
      (let* ((D	'())
	     (S	(make-iqueue-chain D)))
	(iqueue-empty? S))
    => #t)

  (check
      (let* ((D	'())
	     (S	(make-iqueue-chain D)))
	(iqueue-push! S 0)
	(let* ((rv1 (iqueue-top  S))
	       (rv2 (iqueue-pop! S)))
	  (values rv1 rv2 (iqueue-empty? S))))
    => 0 0 #t)

  (check
      (let* ((D	(chain))
	     (S	(make-iqueue-chain D)))
	(iqueue-push! S 0)
	(iqueue-push! S 1)
	(iqueue-push! S 2)
	(let* ((rv1 (iqueue-top  S))
	       (rv2 (iqueue-pop! S))
	       (rv3 (iqueue-top  S))
	       (rv4 (iqueue-pop! S))
	       (rv5 (iqueue-top  S))
	       (rv6 (iqueue-pop! S)))
	  (values rv1 rv2  rv3 rv4  rv5 rv6
		  (iqueue-empty? S))))
    => 0 0 1 1 2 2 #t)

  (check
      (let* ((D	(chain 0 1 2 3))
	     (S	(make-iqueue-chain D)))
	(chain->list (iqueue-chain-first-link S)))
    => '(0 1 2 3))

  (check
      (let* ((D	(chain 0 1 2 3))
	     (S	(make-iqueue-chain D)))
	(chain-link-ref (iqueue-chain-last-link S)))
    => 3)

;;; --------------------------------------------------------------------

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
