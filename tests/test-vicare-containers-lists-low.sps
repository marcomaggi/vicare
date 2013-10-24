;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for lists helpers
;;;Date: Mon Dec 29, 2008
;;;
;;;Abstract
;;;
;;;	This test file  holds tests for low level  helper functions from
;;;	(vicare containers lists low).
;;;
;;;Copyright (c) 2008, 2009, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (except (vicare)
		break)
  (vicare checks)
  (vicare containers lists low))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare containers libraries: low level list functions\n")


;;;; helpers

(define error-message "expected lists of equal length")


(parameterise ((check-test-name 'cars))

  (check
      (%cars '(()))
    => '())

  (check
      (%cars '((1)))
    => '(1))

  (check
      (%cars '((1 2 3)
	       (10 20 30)))
    => '(1 10))

  (check
      (%cars '((1 2 3)
	       (10 20 30)
	       (100 200 300)))
    => '(1 10 100))

  (check
      (guard (exc (else (condition-message exc)))
	(%cars '(()
		 (10 20 30)
		 (100 200 300))))
    => error-message)

  (check
      (guard (exc (else (condition-message exc)))
	(%cars '((1 2 3)
		 ()
		 (100 200 300))))
    => error-message)

  (check
      (guard (exc (else (condition-message exc)))
	(%cars '((1 2 3)
		 (10 20 30)
		 ())))
    => error-message)

;;; --------------------------------------------------------------------

  (check
      (%cars* '(()))
    => '())

  (check
      (%cars* '((1)))
    => '(1))

  (check
      (%cars* '((1 2 3)
		(10 20 30)))
    => '(1 10))

  (check
      (%cars* '((1 2 3)
		(10 20 30)
		(100 200 300)))
    => '(1 10 100))

  (check
      (%cars* '(()
		(10 20 30)
		(100 200 300)))
    => '())

  (check
      (%cars* '((1 2 3)
		()
		(100 200 300)))
    => '())

  (check
      (%cars* '((1 2 3)
		(10 20 30)
		()))
    => '())

  )


(parameterise ((check-test-name 'cdrs))

  (check
      (%cdrs '(()))
    => '())

  (check
      (%cdrs '((1)))
    => '(()))

  (check
      (%cdrs '((1)
	       (10)))
    => '(()
	 ()))

  (check
      (%cdrs '((1)
	       (10)
	       (100)))
    => '(()
	 ()
	 ()))

  (check
      (%cdrs '((1 2 3)
	       (10 20 30)))
    => '((2 3)
	 (20 30)))

  (check
      (%cdrs '((1 2 3)
	       (10 20 30)
	       (100 200 300)))
    => '((2 3)
	 (20 30)
	 (200 300)))

  (check
      (guard (exc (else (condition-message exc)))
	(%cdrs '(()
		 (10 20 30)
		 (100 200 300))))
    => error-message)

  (check
      (guard (exc (else (condition-message exc)))
	(%cdrs '((1 2 3)
		 ()
		 (100 200 300))))
    => error-message)

  (check
      (guard (exc (else (condition-message exc)))
	(%cdrs '((1 2 3)
		 (10 20 30)
		 ())))
    => error-message)

;;; --------------------------------------------------------------------

  (check
      (%cdrs* '(()))
    => '())

  (check
      (%cdrs* '((1)))
    => '(()))

  (check
      (%cdrs* '((1)
		(10)))
    => '(()
	 ()))

  (check
      (%cdrs* '((1)
		(10)
		(100)))
    => '(()
	 ()
	 ()))

  (check
      (%cdrs* '((1 2 3)
		(10 20 30)))
    => '((2 3)
	 (20 30)))

  (check
      (%cdrs* '((1 2 3)
		(10 20 30)
		(100 200 300)))
    => '((2 3)
	 (20 30)
	 (200 300)))

  (check
      (%cdrs* '(()
		(10 20 30)
		(100 200 300)))
    => '())

  (check
      (%cdrs* '((1 2 3)
		()
		(100 200 300)))
    => '())

  (check
      (%cdrs* '((1 2 3)
		(10 20 30)
		()))
    => '())

  )


(parameterise ((check-test-name 'cars+knil))

  (check
      (%cars+knil '(()) 999)
    => '())

  (check
      (%cars+knil '((1)) 999)
    => '(1 999))

  (check
      (%cars+knil '((1 2 3)
		    (10 20 30))
		  999)
    => '(1 10 999))

  (check
      (%cars+knil '((1 2 3)
		    (10 20 30)
		    (100 200 300))
		  999)
    => '(1 10 100 999))

  (check
      (guard (exc (else (condition-message exc)))
	(%cars+knil '(()
		      (10 20 30)
		      (100 200 300))
		    999))
    => error-message)

  (check
      (guard (exc (else (condition-message exc)))
	(%cars+knil '((1 2 3)
		      ()
		      (100 200 300))
		    999))
    => error-message)

  (check
      (guard (exc (else (condition-message exc)))
	(%cars+knil '((1 2 3)
		      (10 20 30)
		      ())
		    999))
    => error-message)

;;; --------------------------------------------------------------------

  (check
      (%cars+knil* '(()) 999)
    => '())

  (check
      (%cars+knil* '((1)) 999)
    => '(1 999))

  (check
      (%cars+knil* '((1 2 3)
		     (10 20 30))
		   999)
    => '(1 10 999))

  (check
      (%cars+knil* '((1 2 3)
		     (10 20 30)
		     (100 200 300))
		   999)
    => '(1 10 100 999))

  (check
      (%cars+knil* '(()
		     (10 20 30)
		     (100 200 300))
		   999)
    => '())

  (check
      (%cars+knil* '((1 2 3)
		     ()
		     (100 200 300))
		   999)
    => '())

  (check
      (%cars+knil* '((1 2 3)
		     (10 20 30)
		     ())
		   999)
    => '())

  )


(parameterise ((check-test-name 'knil+cars))

  (check
      (%knil+cars '(()) 999)
    => '())

  (check
      (%knil+cars '((1)) 999)
    => '(999 1))

  (check
      (%knil+cars '((1 2 3)
		    (10 20 30))
		  999)
    => '(999 1 10))

  (check
      (%knil+cars '((1 2 3)
		    (10 20 30)
		    (100 200 300))
		  999)
    => '(999 1 10 100))

  (check
      (guard (exc (else (condition-message exc)))
	(%knil+cars '(()
		      (10 20 30)
		      (100 200 300))
		    999))
    => error-message)

  (check
      (guard (exc (else (condition-message exc)))
	(%knil+cars '((1 2 3)
		      ()
		      (100 200 300))
		    999))
    => error-message)

  (check
      (guard (exc (else (condition-message exc)))
	(%knil+cars '((1 2 3)
		      (10 20 30)
		      ())
		    999))
    => error-message)

;;; --------------------------------------------------------------------

  (check
      (%knil+cars* '(()) 999)
    => '())

  (check
      (%knil+cars* '((1)) 999)
    => '(999 1))

  (check
      (%knil+cars* '((1 2 3)
		     (10 20 30))
		   999)
    => '(999 1 10))

  (check
      (%knil+cars* '((1 2 3)
		     (10 20 30)
		     (100 200 300))
		   999)
    => '(999 1 10 100))

  (check
      (%knil+cars* '(()
		     (10 20 30)
		     (100 200 300))
		   999)
    => '())

  (check
      (%knil+cars* '((1 2 3)
		     ()
		     (100 200 300))
		   999)
    => '())

  (check
      (%knil+cars* '((1 2 3)
		     (10 20 30)
		     ())
		   999)
    => '())

  )


(parameterise ((check-test-name 'cars/cdrs))

  (check
      (call-with-values
	  (lambda ()
	    (%cars/cdrs '(())))
	list)
    => '(()
	 ()))

  (check
      (call-with-values
	  (lambda ()
	    (%cars/cdrs '((1))))
	list)
    => '((1)
	 (())))

  (check
      (call-with-values
	  (lambda ()
	    (%cars/cdrs '((1)
			  (10))))
	list)
    => '((1 10)
	 (()
	  ())))

  (check
      (call-with-values
	  (lambda ()
	    (%cars/cdrs '((1)
			  (10)
			  (100))))
	list)
    => '((1 10 100)
	 (()
	  ()
	  ())))

  (check
      (call-with-values
	  (lambda ()
	    (%cars/cdrs '((1 2 3)
			  (10 20 30))))
	list)
    => '((1 10)
	 ((2 3)
	  (20 30))))

  (check
      (call-with-values
	  (lambda ()
	    (%cars/cdrs '((1 2 3)
			  (10 20 30)
			  (100 200 300))))
	list)
    => '((1 10 100)
	 ((2 3)
	  (20 30)
	  (200 300))))

  (check
      (guard (exc (else (condition-message exc)))
	(%cars/cdrs '(()
		      (10 20 30)
		      (100 200 300))))
    => error-message)

  (check
      (guard (exc (else (condition-message exc)))
	(%cars/cdrs '((1 2 3)
		      ()
		      (100 200 300))))
    => error-message)

  (check
      (guard (exc (else (condition-message exc)))
	(%cars/cdrs '((1 2 3)
		      (10 20 30)
		      ())))
    => error-message)

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda ()
	    (%cars/cdrs* '(())))
	list)
    => '(()
	 ()))

  (check
      (call-with-values
	  (lambda ()
	    (%cars/cdrs* '((1))))
	list)
    => '((1)
	 (())))

  (check
      (call-with-values
	  (lambda ()
	    (%cars/cdrs* '((1)
			   (10))))
	list)
    => '((1 10)
	 (()
	  ())))

  (check
      (call-with-values
	  (lambda ()
	    (%cars/cdrs* '((1)
			   (10)
			   (100))))
	list)
    => '((1 10 100)
	 (()
	  ()
	  ())))

  (check
      (call-with-values
	  (lambda ()
	    (%cars/cdrs* '((1 2 3)
			   (10 20 30))))
	list)
    => '((1 10)
	 ((2 3)
	  (20 30))))

  (check
      (call-with-values
	  (lambda ()
	    (%cars/cdrs* '((1 2 3)
			   (10 20 30)
			   (100 200 300))))
	list)
    => '((1 10 100)
	 ((2 3)
	  (20 30)
	  (200 300))))

  (check
      (call-with-values
	  (lambda ()
	    (%cars/cdrs* '(()
			   (10 20 30)
			   (100 200 300))))
	list)
    => '(()
	 ()))

  (check
      (call-with-values
	  (lambda ()
	    (%cars/cdrs* '((1 2 3)
			   ()
			   (100 200 300))))
	list)
    => '(()
	 ()))

  (check
      (call-with-values
	  (lambda ()
	    (%cars/cdrs* '((1 2 3)
			   (10 20 30)
			   ())))
	list)
    => '(()
	 ()))

  )


(parameterise ((check-test-name 'cars+knil/cdrs))

  (check
      (call-with-values
	  (lambda ()
	    (%cars+knil/cdrs '(()) 999))
	list)
    => '(()
	 ()))

  (check
      (call-with-values
	  (lambda ()
	    (%cars+knil/cdrs '((1)) 999))
	list)
    => '((1 999)
	 (())))

  (check
      (call-with-values
	  (lambda ()
	    (%cars+knil/cdrs '((1)
			       (10))
			     999))
	list)
    => '((1 10 999)
	 (()
	  ())))

  (check
      (call-with-values
	  (lambda ()
	    (%cars+knil/cdrs '((1)
			       (10)
			       (100))
			     999))
	list)
    => '((1 10 100 999)
	 (()
	  ()
	  ())))

  (check
      (call-with-values
	  (lambda ()
	    (%cars+knil/cdrs '((1 2 3)
			       (10 20 30))
			     999))
	list)
    => '((1 10 999)
	 ((2 3)
	  (20 30))))

  (check
      (call-with-values
	  (lambda ()
	    (%cars+knil/cdrs '((1 2 3)
			       (10 20 30)
			       (100 200 300))
			     999))
	list)
    => '((1 10 100 999)
	 ((2 3)
	  (20 30)
	  (200 300))))

  (check
      (guard (exc (else (condition-message exc)))
	(%cars+knil/cdrs '(()
			   (10 20 30)
			   (100 200 300))
			 999))
    => error-message)

  (check
      (guard (exc (else (condition-message exc)))
	(%cars+knil/cdrs '((1 2 3)
			   ()
			   (100 200 300))
			 999))
    => error-message)

  (check
      (guard (exc (else (condition-message exc)))
	(%cars+knil/cdrs '((1 2 3)
			   (10 20 30)
			   ())
			 999))
    => error-message)

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda ()
	    (%cars+knil/cdrs* '(()) 999))
	list)
    => '(()
	 ()))

  (check
      (call-with-values
	  (lambda ()
	    (%cars+knil/cdrs* '((1)) 999))
	list)
    => '((1 999)
	 (())))

  (check
      (call-with-values
	  (lambda ()
	    (%cars+knil/cdrs* '((1)
				(10))
			      999))
	list)
    => '((1 10 999)
	 (()
	  ())))

  (check
      (call-with-values
	  (lambda ()
	    (%cars+knil/cdrs* '((1)
				(10)
				(100))
			      999))
	list)
    => '((1 10 100 999)
	 (()
	  ()
	  ())))

  (check
      (call-with-values
	  (lambda ()
	    (%cars+knil/cdrs* '((1 2 3)
				(10 20 30))
			      999))
	list)
    => '((1 10 999)
	 ((2 3)
	  (20 30))))

  (check
      (call-with-values
	  (lambda ()
	    (%cars+knil/cdrs* '((1 2 3)
				(10 20 30)
				(100 200 300))
			      999))
	list)
    => '((1 10 100 999)
	 ((2 3)
	  (20 30)
	  (200 300))))

  (check
      (call-with-values
	  (lambda ()
	    (%cars+knil/cdrs* '(()
				(10 20 30)
				(100 200 300))
			      999))
	list)
    => '(()
	 ()))

  (check
      (call-with-values
	  (lambda ()
	    (%cars+knil/cdrs* '((1 2 3)
				()
				(100 200 300))
			      999))
	list)
    => '(()
	 ()))

  (check
      (call-with-values
	  (lambda ()
	    (%cars+knil/cdrs* '((1 2 3)
				(10 20 30)
				())
			      999))
	list)
    => '(()
	 ()))

  )


(parameterise ((check-test-name 'knil+cars/cdrs))

  (check
      (call-with-values
	  (lambda ()
	    (%knil+cars/cdrs '(()) 999))
	list)
    => '(()
	 ()))

  (check
      (call-with-values
	  (lambda ()
	    (%knil+cars/cdrs '((1)) 999))
	list)
    => '((999 1)
	 (())))

  (check
      (call-with-values
	  (lambda ()
	    (%knil+cars/cdrs '((1)
			       (10))
			     999))
	list)
    => '((999 1 10)
	 (()
	  ())))

  (check
      (call-with-values
	  (lambda ()
	    (%knil+cars/cdrs '((1)
			       (10)
			       (100))
			     999))
	list)
    => '((999 1 10 100)
	 (()
	  ()
	  ())))

  (check
      (call-with-values
	  (lambda ()
	    (%knil+cars/cdrs '((1 2 3)
			       (10 20 30))
			     999))
	list)
    => '((999 1 10)
	 ((2 3)
	  (20 30))))

  (check
      (call-with-values
	  (lambda ()
	    (%knil+cars/cdrs '((1 2 3)
			       (10 20 30)
			       (100 200 300))
			     999))
	list)
    => '((999 1 10 100)
	 ((2 3)
	  (20 30)
	  (200 300))))

  (check
      (guard (exc (else (condition-message exc)))
	(%knil+cars/cdrs '(()
			   (10 20 30)
			   (100 200 300))
			 999))
    => error-message)

  (check
      (guard (exc (else (condition-message exc)))
	(%knil+cars/cdrs '((1 2 3)
			   ()
			   (100 200 300))
			 999))
    => error-message)

  (check
      (guard (exc (else (condition-message exc)))
	(%knil+cars/cdrs '((1 2 3)
			   (10 20 30)
			   ())
			 999))
    => error-message)

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda ()
	    (%knil+cars/cdrs* '(()) 999))
	list)
    => '(()
	 ()))

  (check
      (call-with-values
	  (lambda ()
	    (%knil+cars/cdrs* '((1)) 999))
	list)
    => '((999 1)
	 (())))

  (check
      (call-with-values
	  (lambda ()
	    (%knil+cars/cdrs* '((1)
				(10))
			      999))
	list)
    => '((999 1 10)
	 (()
	  ())))

  (check
      (call-with-values
	  (lambda ()
	    (%knil+cars/cdrs* '((1)
				(10)
				(100))
			      999))
	list)
    => '((999 1 10 100)
	 (()
	  ()
	  ())))

  (check
      (call-with-values
	  (lambda ()
	    (%knil+cars/cdrs* '((1 2 3)
				(10 20 30))
			      999))
	list)
    => '((999 1 10)
	 ((2 3)
	  (20 30))))

  (check
      (call-with-values
	  (lambda ()
	    (%knil+cars/cdrs* '((1 2 3)
				(10 20 30)
				(100 200 300))
			      999))
	list)
    => '((999 1 10 100)
	 ((2 3)
	  (20 30)
	  (200 300))))

  (check
      (call-with-values
	  (lambda ()
	    (%knil+cars/cdrs* '(()
				(10 20 30)
				(100 200 300))
			      999))
	list)
    => '(()
	 ()))

  (check
      (call-with-values
	  (lambda ()
	    (%knil+cars/cdrs* '((1 2 3)
				()
				(100 200 300))
			      999))
	list)
    => '(()
	 ()))

  (check
      (call-with-values
	  (lambda ()
	    (%knil+cars/cdrs* '((1 2 3)
				(10 20 30)
				())
			      999))
	list)
    => '(()
	 ()))

  )


;;;; done

(check-report)

;;; end of file
