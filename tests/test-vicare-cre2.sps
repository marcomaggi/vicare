;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: built in binding to CRE2
;;;Date: Fri Jan  6, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (rename (vicare)
		(parameterize	parametrise))
  (prefix (vicare cre2) cre2.)
  (checks))

(unless (cre2.enabled?)
  (exit 0))

(check-set-mode! 'report-failed)
(display "*** testing Vicare CRE2 binding\n")


(parametrise ((check-test-name	'version))

  (check
      (fixnum? (cre2.version-interface-current))
    => #t)

  (check
      (fixnum? (cre2.version-interface-revision))
    => #t)

  (check
      (fixnum? (cre2.version-interface-age))
    => #t)
  #t)


(parametrise ((check-test-name	'options))

  (check
      (let ((opts (cre2.make-options)))
	(cre2.options? opts))
    => #t)

  (check
      (let ((opts (cre2.make-options)))
	(cre2.delete-options opts)
	(cre2.delete-options opts)
	(cre2.delete-options opts)
	(cre2.options? opts))
    => #t)

  (when #f
    (check-display (cre2.make-options))
    (check-newline))

;;; --------------------------------------------------------------------

  (check
      (let ((opts (cre2.make-options)))
	(cre2.set-posix-syntax! opts #t)
	(cre2.posix-syntax? opts))
    => #t)

  (check
      (let ((opts (cre2.make-options)))
	(cre2.set-posix-syntax! opts #f)
	(cre2.posix-syntax? opts))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((opts (cre2.make-options)))
	(cre2.set-longest-match! opts #t)
	(cre2.longest-match? opts))
    => #t)

  (check
      (let ((opts (cre2.make-options)))
	(cre2.set-longest-match! opts #f)
	(cre2.longest-match? opts))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((opts (cre2.make-options)))
	(cre2.set-log-errors! opts #t)
	(cre2.log-errors? opts))
    => #t)

  (check
      (let ((opts (cre2.make-options)))
	(cre2.set-log-errors! opts #f)
	(cre2.log-errors? opts))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((opts (cre2.make-options)))
	(cre2.set-literal! opts #t)
	(cre2.literal? opts))
    => #t)

  (check
      (let ((opts (cre2.make-options)))
	(cre2.set-literal! opts #f)
	(cre2.literal? opts))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((opts (cre2.make-options)))
	(cre2.set-never-nl! opts #t)
	(cre2.never-nl? opts))
    => #t)

  (check
      (let ((opts (cre2.make-options)))
	(cre2.set-never-nl! opts #f)
	(cre2.never-nl? opts))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((opts (cre2.make-options)))
	(cre2.set-case-sensitive! opts #t)
	(cre2.case-sensitive? opts))
    => #t)

  (check
      (let ((opts (cre2.make-options)))
	(cre2.set-case-sensitive! opts #f)
	(cre2.case-sensitive? opts))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((opts (cre2.make-options)))
	(cre2.set-perl-classes! opts #t)
	(cre2.perl-classes? opts))
    => #t)

  (check
      (let ((opts (cre2.make-options)))
	(cre2.set-perl-classes! opts #f)
	(cre2.perl-classes? opts))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((opts (cre2.make-options)))
	(cre2.set-word-boundary! opts #t)
	(cre2.word-boundary? opts))
    => #t)

  (check
      (let ((opts (cre2.make-options)))
	(cre2.set-word-boundary! opts #f)
	(cre2.word-boundary? opts))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((opts (cre2.make-options)))
	(cre2.set-one-line! opts #t)
	(cre2.one-line? opts))
    => #t)

  (check
      (let ((opts (cre2.make-options)))
	(cre2.set-one-line! opts #f)
	(cre2.one-line? opts))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((opts (cre2.make-options)))
	(cre2.set-max-mem! opts 1024)
	(cre2.max-mem opts))
    => 1024)

  #t)


(parametrise ((check-test-name	'regexps))

  (check
      (let ((rex (cre2.make-regexp "ciao|hello" (cre2.make-options))))
	(cre2.regexp? rex))
    => #t)

  (check
      (let ((rex (cre2.make-regexp "ciao|hello")))
	(cre2.regexp? rex))
    => #t)

  (check
      (let ((rex (cre2.make-regexp (string->utf8 "ciao|hello"))))
	(cre2.regexp? rex))
    => #t)

  (check
      (let ((rex (cre2.make-regexp "ciao|hello")))
	(cre2.delete-regexp rex)
	(cre2.delete-regexp rex)
	(cre2.delete-regexp rex)
	(cre2.regexp? rex))
    => #t)

  #t)


(parametrise ((check-test-name	'match))

  (check
      (let ((rex (cre2.make-regexp "ciao|hello")))
	(cre2.match rex "ciao" #f #f 'unanchored))
    => '#((0 . 4)))

  (check
      (let ((rex (cre2.make-regexp "ciao|hello")))
	(cre2.match rex "ohayo" #f #f 'unanchored))
    => #f)

  (check
      (let ((rex (cre2.make-regexp "ciao|hello")))
	(cre2.match rex "hello" #f #f 'unanchored))
    => '#((0 . 5)))

  (check
      (let ((rex (cre2.make-regexp "ci(ao)|hello")))
	(cre2.match rex "ciao" #f #f 'unanchored))
    => '#((0 . 4)
	  (2 . 4)))

  (check
      (let ((rex (cre2.make-regexp "c(i(ao))|hello")))
	(cre2.match rex "ciao" #f #f 'unanchored))
    => '#((0 . 4)
	  (1 . 4)
	  (2 . 4)))

  #t)


;;;; done

(check-report)

;;; end of file
