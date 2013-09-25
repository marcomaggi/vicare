;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for identifier alists
;;;Date: Tue Sep 24, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (vicare language-extensions identifier-alists)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: identifier alists\n")


(parametrise ((check-test-name	'cons))

  (check
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao 123 table)))
        table)
    (=> syntax=?)
    (list (cons #'ciao 123)))

  (check
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao 123 table))
	     (table	(identifier-alist-cons #'hello 456 table))
	     (table	(identifier-alist-cons #'salut 789 table)))
        table)
    (=> syntax=?)
    (list (cons #'salut 789)
	  (cons #'hello 456)
	  (cons #'ciao 123)))

  #t)


(parametrise ((check-test-name	'replacing))

  (check
      (let* ((table	'()))
	(identifier-alist-cons-and-replace #'hello 0 table))
    (=> syntax=?)
    (list (cons #'hello 0)))

  (check	;insertion only
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao  123 table))
	     (table	(identifier-alist-cons #'salut 789 table)))
	(identifier-alist-cons-and-replace #'hello 0 table))
    (=> syntax=?)
    (list (cons #'hello 0)
	  (cons #'salut 789)
	  (cons #'ciao 123)))

  (check	;true replacing
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao  123 table))
	     (table	(identifier-alist-cons #'hello 456 table))
	     (table	(identifier-alist-cons #'salut 789 table)))
	(identifier-alist-cons-and-replace #'hello 0 table))
    (=> syntax=?)
    (list (cons #'hello 0)
	  (cons #'salut 789)
	  (cons #'ciao 123)))

  #t)


(parametrise ((check-test-name	'new))

  (check
      (let* ((table	'()))
	(identifier-alist-new #'hello 0 table))
    (=> syntax=?)
    (list (cons #'hello 0)))

  (check	;insertion only
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao  123 table))
	     (table	(identifier-alist-cons #'salut 789 table)))
	(identifier-alist-new #'hello 0 table))
    (=> syntax=?)
    (list (cons #'hello 0)
	  (cons #'salut 789)
	  (cons #'ciao 123)))

  (check	;true replacing
      (guard (E ((assertion-violation? E)
		 (condition-message E))
		(else E))
	(let* ((table	'())
	       (table	(identifier-alist-cons #'ciao  123 table))
	       (table	(identifier-alist-cons #'hello 456 table))
	       (table	(identifier-alist-cons #'salut 789 table)))
	  (identifier-alist-new #'hello 0 table)))
    => "key already present in identifier alist")

  #t)


(parametrise ((check-test-name	'ref))

  (check
      (let* ((table	'()))
	(identifier-alist-ref table #'hello #f))
    => #f)

  (check
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao  123 table)))
	(identifier-alist-ref table #'hello #f))
    => #f)

  (check
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao  123 table))
	     (table	(identifier-alist-cons #'salut 789 table)))
	(identifier-alist-ref table #'hello #f))
    => #f)

  (check
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao  123 table))
	     (table	(identifier-alist-cons #'hello 456 table))
	     (table	(identifier-alist-cons #'salut 789 table)))
	(identifier-alist-ref table #'ciao #f))
    => 123)

  (check
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao  123 table))
	     (table	(identifier-alist-cons #'hello 456 table))
	     (table	(identifier-alist-cons #'salut 789 table)))
	(identifier-alist-ref table #'hello #f))
    => 456)

  (check
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao  123 table))
	     (table	(identifier-alist-cons #'hello 456 table))
	     (table	(identifier-alist-cons #'salut 789 table)))
	(identifier-alist-ref table #'salut #f))
    => 789)

  #t)


(parametrise ((check-test-name	'setting))

  (check
      (guard (E ((assertion-violation? E)
		 (condition-message E))
		(else E))
	(let* ((table	'()))
	  (identifier-alist-set! table #'hello 0)))
    => "expected identifier alist entry does not exist")

  (check
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao  123 table)))
	(identifier-alist-set! table #'ciao 0))
    (=> syntax=?)
    (list (cons #'ciao 0)))

  (check
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao  123 table))
	     (table	(identifier-alist-cons #'hello 456 table))
	     (table	(identifier-alist-cons #'salut 789 table)))
	(identifier-alist-set! table #'ciao 0))
    (=> syntax=?)
    (list (cons #'salut 789)
	  (cons #'hello 456)
	  (cons #'ciao  0)))

  (check
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao  123 table))
	     (table	(identifier-alist-cons #'hello 456 table))
	     (table	(identifier-alist-cons #'salut 789 table)))
	(identifier-alist-set! table #'hello 0))
    (=> syntax=?)
    (list (cons #'salut 789)
	  (cons #'hello 0)
	  (cons #'ciao  123)))

  (check
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao  123 table))
	     (table	(identifier-alist-cons #'hello 456 table))
	     (table	(identifier-alist-cons #'salut 789 table)))
	(identifier-alist-set! table #'salut 0))
    (=> syntax=?)
    (list (cons #'salut 0)
	  (cons #'hello 456)
	  (cons #'ciao  123)))

  #t)


(parametrise ((check-test-name	'removing))

  (check
      (let* ((table	'()))
	(identifier-alist-remove table #'hello))
    (=> syntax=?)
    '())

  (check
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao  123 table))
	     (table	(identifier-alist-cons #'salut 789 table)))
	(identifier-alist-remove table #'hello))
    (=> syntax=?)
    (list (cons #'salut 789)
	  (cons #'ciao 123)))

  (check
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao  123 table))
	     (table	(identifier-alist-cons #'hello 456 table))
	     (table	(identifier-alist-cons #'salut 789 table)))
	(identifier-alist-remove table #'ciao))
    (=> syntax=?)
    (list (cons #'salut 789)
	  (cons #'hello 456)))

  (check
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao  123 table))
	     (table	(identifier-alist-cons #'hello 456 table))
	     (table	(identifier-alist-cons #'salut 789 table)))
	(identifier-alist-remove table #'hello))
    (=> syntax=?)
    (list (cons #'salut 789)
	  (cons #'ciao 123)))

  (check
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao  123 table))
	     (table	(identifier-alist-cons #'hello 456 table))
	     (table	(identifier-alist-cons #'salut 789 table)))
	(identifier-alist-remove table #'salut))
    (=> syntax=?)
    (list (cons #'hello 456)
	  (cons #'ciao 123)))

  #t)


(parametrise ((check-test-name	'exists))

  (check
      (let* ((table	'()))
	(identifier-alist-exists table #'hello))
    => #f)

  (check
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao  123 table))
	     (table	(identifier-alist-cons #'salut 789 table)))
	(identifier-alist-exists table #'hello))
    => #f)

  (check
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao  123 table))
	     (table	(identifier-alist-cons #'hello 456 table))
	     (table	(identifier-alist-cons #'salut 789 table)))
	(identifier-alist-exists table #'ciao))
    (=> syntax=?)
    (cons #'ciao 123))

  (check
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao  123 table))
	     (table	(identifier-alist-cons #'hello 456 table))
	     (table	(identifier-alist-cons #'salut 789 table)))
	(identifier-alist-exists table #'hello))
    (=> syntax=?)
    (cons #'hello 456))

  (check
      (let* ((table	'())
	     (table	(identifier-alist-cons #'ciao  123 table))
	     (table	(identifier-alist-cons #'hello 456 table))
	     (table	(identifier-alist-cons #'salut 789 table)))
	(identifier-alist-exists table #'salut))
    (=> syntax=?)
    (cons #'salut 789))

  #t)


;;;; done

(check-report)

;;; end of file
