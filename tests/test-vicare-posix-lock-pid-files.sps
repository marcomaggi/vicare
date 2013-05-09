;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for lock pid files
;;;Date: Thu May  9, 2013
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
  (prefix (vicare posix) px.)
  (vicare posix lock-pid-files)
  (vicare platform constants)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: POSIX lock PID files\n")


(parametrise ((check-test-name	'basic))

  (define-constant CONTENTS
    (string-append (number->string (px.getpid)) "\n"))

  (check	;plain body evaluation
      (with-lock-pid-file "lock-pid-file.001" values
	(lambda ()
	  'ok))
    => 'ok)

  (check	;lock file exists while evaluating body
      (with-lock-pid-file "lock-pid-file.002" values
	(lambda ()
	  (file-exists? "lock-pid-file.002")))
    => #t)

  (check	;lock file contents
      (with-lock-pid-file "lock-pid-file.003" values
	(lambda ()
	  (with-input-from-file "lock-pid-file.003"
	    (lambda ()
	      (get-string-all (current-input-port))))))
    => CONTENTS)

  (check	;error locking twice
      (guard (E ((errno-condition? E)
		 (condition-errno E))
		(else E))
	(with-lock-pid-file "lock-pid-file.004" values
	  (lambda ()
	    (with-lock-pid-file "lock-pid-file.004" values
	      (lambda ()
		'error)))))
    => EEXIST)

  (check	;cleanup of lock file when body raises exception
      (guard (E (else
		 (list E (file-exists? "lock-pid-file.005"))))
	(with-lock-pid-file "lock-pid-file.005" values
	  (lambda ()
	    (raise 'error))))
    => '(error #f))

  #t)


(parametrise ((check-test-name	'logging))

  (check
      (with-result
       (with-lock-pid-file "lock-pid-file.101"
	   (lambda (template . args)
	     (add-result (apply format template args)))
	 (lambda ()
	   'ok)))
    => '(ok
	 ("creating lock PID file: lock-pid-file.101"
	  "removing lock PID file: lock-pid-file.101")))

  (check
      (with-result
       (guard (E ((errno-condition? E)
		  (condition-errno E))
		 (else E))
	 (with-lock-pid-file "lock-pid-file.102"
	     (lambda (template . args)
	       (add-result (apply format template args)))
	   (lambda ()
	     (with-lock-pid-file "lock-pid-file.102"
		 (lambda (template . args)
		   (add-result (apply format template args)))
	       (lambda ()
		 'error))))))
    => `(,EEXIST
	 ("creating lock PID file: lock-pid-file.102"
	  "creating lock PID file: lock-pid-file.102"
	  "while creating lock PID file: EEXIST: File exists"
	  "removing lock PID file: lock-pid-file.102")))

;;; --------------------------------------------------------------------

  (check	;cleanup of lock file when body raises exception
      (with-result
       (guard (E (else
		  (list E (file-exists? "lock-pid-file.103"))))
	 (with-lock-pid-file "lock-pid-file.103"
	     (lambda (template . args)
	       (add-result (apply format template args)))
	   (lambda ()
	     (raise 'fail)))))
    => '((fail #f)
	 ("creating lock PID file: lock-pid-file.103"
	  "removing lock PID file: lock-pid-file.103")))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-lock-pid-file 'scheme-indent-function 2)
;; End:
