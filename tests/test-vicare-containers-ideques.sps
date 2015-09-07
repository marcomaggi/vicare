;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for ideques
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


#!r6rs
(import (vicare)
  (vicare containers deques)
  (vicare containers dynamic-arrays)
  (vicare containers chains)
  (vicare containers ideques)
  (vicare containers ideques deques)
  (vicare containers ideques dynamic-arrays)
  (vicare containers ideques chains)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: tests for ideques\n")


(parametrise ((check-test-name	'deques))

  (check
      (let* ((D	(deque 0 1 2 3))
	     (S	(make-ideque-deque D)))
	(ideque-empty? S))
    => #f)

  (check
      (let* ((D	(deque))
	     (S	(make-ideque-deque D)))
	(ideque-empty? S))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((D	(deque))
	     (S	(make-ideque-deque D)))
	(ideque-push-rear! S 0)
	(let* ((rv1 (ideque-front  S))
	       (rv2 (ideque-pop-front! S)))
	  (values rv1 rv2 (ideque-empty? S))))
    => 0 0 #t)

  (check
      (let* ((D	(deque))
	     (S	(make-ideque-deque D)))
	(ideque-push-front! S 0)
	(let* ((rv1 (ideque-rear  S))
	       (rv2 (ideque-pop-rear! S)))
	  (values rv1 rv2 (ideque-empty? S))))
    => 0 0 #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((D	(deque))
	     (S	(make-ideque-deque D)))
	(ideque-push-rear! S 0)
	(ideque-push-rear! S 1)
	(ideque-push-rear! S 2)
	(let* ((rv1 (ideque-front  S))
	       (rv2 (ideque-pop-front! S))
	       (rv3 (ideque-front  S))
	       (rv4 (ideque-pop-front! S))
	       (rv5 (ideque-front  S))
	       (rv6 (ideque-pop-front! S)))
	  (values rv1 rv2  rv3 rv4  rv5 rv6
		  (ideque-empty? S))))
    => 0 0 1 1 2 2 #t)

  (check
      (let* ((D	(deque))
	     (S	(make-ideque-deque D)))
	(ideque-push-front! S 0)
	(ideque-push-front! S 1)
	(ideque-push-front! S 2)
	(let* ((rv1 (ideque-rear  S))
	       (rv2 (ideque-pop-rear! S))
	       (rv3 (ideque-rear  S))
	       (rv4 (ideque-pop-rear! S))
	       (rv5 (ideque-rear  S))
	       (rv6 (ideque-pop-rear! S)))
	  (values rv1 rv2  rv3 rv4  rv5 rv6
		  (ideque-empty? S))))
    => 0 0 1 1 2 2 #t)

;;; --------------------------------------------------------------------

  #t)


(parametrise ((check-test-name	'dynamic-arrays))

  (check
      (let* ((D	(dynamic-array 0 1 2 3))
	     (S	(make-ideque-dynamic-array D)))
	(ideque-empty? S))
    => #f)

  (check
      (let* ((D	(dynamic-array))
	     (S	(make-ideque-dynamic-array D)))
	(ideque-empty? S))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((D	(dynamic-array))
	     (S	(make-ideque-dynamic-array D)))
	(ideque-push-rear! S 0)
	(let* ((rv1 (ideque-front  S))
	       (rv2 (ideque-pop-front! S)))
	  (values rv1 rv2 (ideque-empty? S))))
    => 0 0 #t)

  (check
      (let* ((D	(dynamic-array))
	     (S	(make-ideque-dynamic-array D)))
	(ideque-push-front! S 0)
	(let* ((rv1 (ideque-rear  S))
	       (rv2 (ideque-pop-rear! S)))
	  (values rv1 rv2 (ideque-empty? S))))
    => 0 0 #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((D	(dynamic-array))
	     (S	(make-ideque-dynamic-array D)))
	(ideque-push-rear! S 0)
	(ideque-push-rear! S 1)
	(ideque-push-rear! S 2)
	(let* ((rv1 (ideque-front  S))
	       (rv2 (ideque-pop-front! S))
	       (rv3 (ideque-front  S))
	       (rv4 (ideque-pop-front! S))
	       (rv5 (ideque-front  S))
	       (rv6 (ideque-pop-front! S)))
	  (values rv1 rv2  rv3 rv4  rv5 rv6
		  (ideque-empty? S))))
    => 0 0 1 1 2 2 #t)

  (check
      (let* ((D	(dynamic-array))
	     (S	(make-ideque-dynamic-array D)))
	(ideque-push-front! S 0)
	(ideque-push-front! S 1)
	(ideque-push-front! S 2)
	(let* ((rv1 (ideque-rear  S))
	       (rv2 (ideque-pop-rear! S))
	       (rv3 (ideque-rear  S))
	       (rv4 (ideque-pop-rear! S))
	       (rv5 (ideque-rear  S))
	       (rv6 (ideque-pop-rear! S)))
	  (values rv1 rv2  rv3 rv4  rv5 rv6
		  (ideque-empty? S))))
    => 0 0 1 1 2 2 #t)

;;; --------------------------------------------------------------------

  #t)


(parametrise ((check-test-name	'chains))

  (check
      (let* ((D	(chain 0 1 2 3))
	     (S	(make-ideque-chain D)))
	(ideque-empty? S))
    => #f)

  (check
      (let* ((D	'())
	     (S	(make-ideque-chain D)))
	(ideque-empty? S))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((D	'())
	     (S	(make-ideque-chain D)))
	(ideque-push-rear! S 0)
	(let* ((rv1 (ideque-front  S))
	       (rv2 (ideque-pop-front! S)))
	  (values rv1 rv2 (ideque-empty? S))))
    => 0 0 #t)

  (check
      (let* ((D	(chain))
	     (S	(make-ideque-chain D)))
	(ideque-push-front! S 0)
	(let* ((rv1 (ideque-rear  S))
	       (rv2 (ideque-pop-rear! S)))
	  (values rv1 rv2 (ideque-empty? S))))
    => 0 0 #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((D	(chain))
	     (S	(make-ideque-chain D)))
	(ideque-push-rear! S 0)
	(ideque-push-rear! S 1)
	(ideque-push-rear! S 2)
	(let* ((rv1 (ideque-front  S))
	       (rv2 (ideque-pop-front! S))
	       (rv3 (ideque-front  S))
	       (rv4 (ideque-pop-front! S))
	       (rv5 (ideque-front  S))
	       (rv6 (ideque-pop-front! S)))
	  (values rv1 rv2  rv3 rv4  rv5 rv6
		  (ideque-empty? S))))
    => 0 0 1 1 2 2 #t)

  (check
      (let* ((D	(chain))
	     (S	(make-ideque-chain D)))
	(ideque-push-front! S 0)
	(ideque-push-front! S 1)
	(ideque-push-front! S 2)
	(let* ((rv1 (ideque-rear  S))
	       (rv2 (ideque-pop-rear! S))
	       (rv3 (ideque-rear  S))
	       (rv4 (ideque-pop-rear! S))
	       (rv5 (ideque-rear  S))
	       (rv6 (ideque-pop-rear! S)))
	  (values rv1 rv2  rv3 rv4  rv5 rv6
		  (ideque-empty? S))))
    => 0 0 1 1 2 2 #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((D	(chain 0 1 2 3))
	     (S	(make-ideque-chain D)))
	(chain->list (ideque-chain-first-link S)))
    => '(0 1 2 3))

  (check
      (let* ((D	(chain 0 1 2 3))
	     (S	(make-ideque-chain D)))
	(chain-link-ref (ideque-chain-last-link S)))
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
