;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: test the Rabin-Karp string search
;;;Date: Wed May 11, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (vicare containers strings rabin-karp)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare containers: Rabin-Karp string search\n")


(parametrise ((check-test-name	'basic))

  (let ((text   "abacadabrabracabracadabrabrabracad"))
    (define-syntax doit
      (syntax-rules ()
	((_ ?pattern ?expected-offset)
	 (doit ?pattern ?expected-offset ?pattern))
	((_ ?pattern ?expected-offset ?expected-pattern)
	 (check
	     (let* ((pattern ?pattern)
		    (search  (make-rabin-karp pattern))
		    (offset  (rabin-karp-search search text)))
	       (list offset (and offset (substring text offset (+ offset (string-length pattern))))))
	   => (list ?expected-offset ?expected-pattern)))))

    (doit "abracadabra"	14)
    (doit "rab"		 8)
    (doit "bcara"	#f #f)
    (doit "rabrabracad"	23)
    (doit "abacad"	 0)

    (check
	(let* ((pattern  "rab")
	       (search   (make-rabin-karp pattern 1000 251))
	       (offset   (rabin-karp-search search text)))
	  (list offset (and offset (substring text offset (+ offset (string-length pattern))))))
      => (list 8 "rab"))

    #f)

  (check
      (let ((search (make-rabin-karp "ciao")))
	(rabin-karp-search search "012345ciao"))
    => 6)

  (check
      (let ((search (make-rabin-karp "hello")))
	(rabin-karp-search search "012345ciao"))
    => #f)

  (check
      (let ((search (make-rabin-karp "ciao")))
	(rabin-karp-search search "hello world"))
    => #f)

  #t)


;;;; done

(check-report)

;;; end of file
