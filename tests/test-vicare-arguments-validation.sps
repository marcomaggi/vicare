;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for arguments validation library
;;;Date: Mon Oct  1, 2012
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
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (vicare syntactic-extensions)
  (prefix (vicare posix) px.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare arguments validation library\n")


(parametrise ((check-test-name	'config))

  (check
      (eval 'config.arguments-validation
	    (environment '(prefix (vicare installation-configuration)
				  config.)))
    => #t)

  (check
      (begin
	(px.setenv "VICARE_ARGUMENTS_VALIDATION" "yes" #t)
	(eval 'config.arguments-validation
	      (environment '(prefix (vicare installation-configuration)
				    config.))))
    => #t)

  (check
      (begin
	(px.setenv "VICARE_ARGUMENTS_VALIDATION" "no" #t)
	(eval 'config.arguments-validation
	      (environment '(prefix (vicare installation-configuration)
				    config.))))
    => #f)

  (check
      (begin
	(px.setenv "VICARE_ARGUMENTS_VALIDATION" "1" #t)
	(eval 'config.arguments-validation
	      (environment '(prefix (vicare installation-configuration)
				    config.))))
    => #t)

  (check
      (begin
	(px.setenv "VICARE_ARGUMENTS_VALIDATION" "0" #t)
	(eval 'config.arguments-validation
	      (environment '(prefix (vicare installation-configuration)
				    config.))))
    => #f)

  (px.setenv "VICARE_ARGUMENTS_VALIDATION" "yes" #t))


(parametrise ((check-test-name	'validate-fixnums))


  #t)


;;;; done

(check-report)

;;; end of file
