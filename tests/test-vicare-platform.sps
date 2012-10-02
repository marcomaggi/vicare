;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for platform constants related features
;;;Date: Tue Oct  2, 2012
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
(import (vicare)
  (vicare platform constants)
  (vicare platform utilities)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare platform utilities\n")


;;;; helpers

(define-syntax catch
  (syntax-rules ()
    ((_ print? . ?body)
     (guard (E ((assertion-violation? E)
		(when print?
		  (check-pretty-print (condition-message E)))
		(condition-irritants E))
	       (else E))
       (begin . ?body)))))

(define-syntax catch-assertion
  (syntax-rules ()
    ((_ ?print-message . ?body)
     (guard (E
	     ((assertion-violation? E)
	      (when ?print-message
		(check-pretty-print (condition-message E)))
	      (condition-irritants E))
	     (else E))
       (let () . ?body)))))


(parametrise ((check-test-name	'symbols))

  (check
      (errno-code->symbol EINVAL)
    => 'EINVAL)

  (check
      (errno-code->symbol (greatest-fixnum))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (posix-signal->symbol SIGTERM)
    => 'SIGTERM)

  (check
      (posix-signal->symbol (greatest-fixnum))
    => #f)

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'case-errno	'scheme-indent-function 1)
;; eval: (put 'catch		'scheme-indent-function 1)
;; eval: (put 'catch-assertion	'scheme-indent-function 1)
;; End:
