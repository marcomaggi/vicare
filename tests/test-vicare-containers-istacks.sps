;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for istacks
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
  (vicare containers deques)
  (vicare containers stacks)
  (vicare containers dynamic-arrays)
  (vicare containers chains)
  (vicare containers ilists)
  (prefix (vicare containers ralists) ra)
  (vicare containers istacks)
  (vicare containers istacks deques)
  (vicare containers istacks stacks)
  (vicare containers istacks dynamic-arrays)
  (vicare containers istacks chains)
  (vicare containers istacks lists)
  (vicare containers istacks ilists)
  (vicare containers istacks ralists)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: tests for istacks\n")


(parametrise ((check-test-name	'lists))

  (check
      (let* ((D	(list 0 1 2 3))
	     (S	(make-istack-list D)))
	(istack-empty? S))
    => #f)

  (check
      (let* ((D	'())
	     (S	(make-istack-list D)))
	(istack-empty? S))
    => #t)

  (check
      (let* ((D	'())
	     (S	(make-istack-list D)))
	(istack-push! S 0)
	(let* ((rv1 (istack-top  S))
	       (rv2 (istack-pop! S)))
	  (values rv1 rv2 (istack-empty? S))))
    => 0 0 #t)

;;; --------------------------------------------------------------------

  #t)


(parametrise ((check-test-name	'ilists))

  (check
      (let* ((D	(ilist 0 1 2 3))
	     (S	(make-istack-ilist D)))
	(istack-empty? S))
    => #f)

  (check
      (let* ((D	'())
	     (S	(make-istack-ilist D)))
	(istack-empty? S))
    => #t)

  (check
      (let* ((D	'())
	     (S	(make-istack-ilist D)))
	(istack-push! S 0)
	(let* ((rv1 (istack-top  S))
	       (rv2 (istack-pop! S)))
	  (values rv1 rv2 (istack-empty? S))))
    => 0 0 #t)

;;; --------------------------------------------------------------------

  #t)


(parametrise ((check-test-name	'ralists))

  (check
      (let* ((D	(ralist 0 1 2 3))
	     (S	(make-istack-ralist D)))
	(istack-empty? S))
    => #f)

  (check
      (let* ((D	'())
	     (S	(make-istack-ralist D)))
	(istack-empty? S))
    => #t)

  (check
      (let* ((D	'())
	     (S	(make-istack-ralist D)))
	(istack-push! S 0)
	(let* ((rv1 (istack-top  S))
	       (rv2 (istack-pop! S)))
	  (values rv1 rv2 (istack-empty? S))))
    => 0 0 #t)

;;; --------------------------------------------------------------------

  #t)


(parametrise ((check-test-name	'deques))

  (check
      (let* ((D	(deque 0 1 2 3))
	     (S	(make-istack-deque D)))
	(istack-empty? S))
    => #f)

  (check
      (let* ((D	(deque))
	     (S	(make-istack-deque D)))
	(istack-empty? S))
    => #t)

  (check
      (let* ((D	(deque))
	     (S	(make-istack-deque D)))
	(istack-push! S 0)
	(let* ((rv1 (istack-top  S))
	       (rv2 (istack-pop! S)))
	  (values rv1 rv2 (istack-empty? S))))
    => 0 0 #t)

;;; --------------------------------------------------------------------

  #t)


(parametrise ((check-test-name	'dynamic-arrays))

  (check
      (let* ((D	(dynamic-array 0 1 2 3))
	     (S	(make-istack-dynamic-array D)))
	(istack-empty? S))
    => #f)

  (check
      (let* ((D	(dynamic-array))
	     (S	(make-istack-dynamic-array D)))
	(istack-empty? S))
    => #t)

  (check
      (let* ((D	(dynamic-array))
	     (S	(make-istack-dynamic-array D)))
	(istack-push! S 0)
	(let* ((rv1 (istack-top  S))
	       (rv2 (istack-pop! S)))
	  (values rv1 rv2 (istack-empty? S))))
    => 0 0 #t)

;;; --------------------------------------------------------------------

  #t)


(parametrise ((check-test-name	'chains))

  (check
      (let* ((D	(chain 0 1 2 3))
	     (S	(make-istack-chain D)))
	(istack-empty? S))
    => #f)

  (check
      (let* ((D	'())
	     (S	(make-istack-chain D)))
	(istack-empty? S))
    => #t)

  (check
      (let* ((D	'())
	     (S	(make-istack-chain D)))
	(istack-push! S 0)
	(let* ((rv1 (istack-top  S))
	       (rv2 (istack-pop! S)))
	  (values rv1 rv2 (istack-empty? S))))
    => 0 0 #t)

;;; --------------------------------------------------------------------

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
