;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for ENSURE syntax
;;;Date: Wed Feb 25, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare language-extensions ensure)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: ENSURE syntax\n")


(parametrise ((check-test-name	'basic))

  (check
      (with-result
	(let ((flag #f))
	  (ensure flag
	      (by
	       (add-result 'by)
	       (set! flag #t)))))
    => '((by)))

  (check
      (with-result
	(try
	    (let ((flag #f))
	      (ensure flag
		  (by
		   (add-result 'by))))
	  (catch E
	    ((&ensure)
	     (add-result 'catch-ensure)
	     #t)
	    (else
	     (add-result 'catch-else)
	     E))))
    => '(#t (by catch-ensure)))

;;; --------------------------------------------------------------------

  (check	;else clause
      (with-result
	(let ((flag #f))
	  (ensure flag
	      (by
	       (add-result 'by))
	    (else-by
	     (add-result 'else-by-1))
	    (else
	     (add-result 'else)))))
    => '((by else-by-1 else)))

  (check	;no else clause, success
      (with-result
	(let ((flag #f))
	  (ensure flag
	      (by
	       (add-result 'by))
	    (else-by
	     (add-result 'else-by-1))
	    (else-by
	     (add-result 'else-by-2)
	     (set! flag #t)))))
    => '((by else-by-1 else-by-2)))

  (check	;no else clause, failure
      (with-result
	(try
	    (let ((flag #f))
	      (ensure flag
		  (by
		   (add-result 'by))
		(else-by
		 (add-result 'else-by-1))
		(else-by
		 (add-result 'else-by-2))))
	  (catch E
	    ((&ensure)
	     (add-result 'catch-ensure)
	     #t)
	    (else
	     (add-result 'catch-else)
	     E))))
    => '(#t (by else-by-1 else-by-2 catch-ensure)))

;;; --------------------------------------------------------------------

  (check	;exception from clauses
      (with-result
	(try
	    (let ((flag #f))
	      (ensure flag
		  (by
		   (add-result 'by))
		(else-by
		 (add-result 'else-by-1)
		 (raise 123))
		(else-by
		 (add-result 'else-by-2))))
	  (catch E
	    ((&ensure)
	     (add-result 'catch-ensure)
	     #t)
	    (else
	     (add-result 'catch-else)
	     E))))
    => '(123 (by else-by-1 catch-else)))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
