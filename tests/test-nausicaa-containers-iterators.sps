;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for iterators
;;;Date: Thu Jul 21, 2011
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
(import (nausicaa)
  (nausicaa containers iterators)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa containers: iterators\n")


#;(parametrise ((check-test-name	'example-string))

  (define-class <simple-string-iterator>
    (nongenerative example:<simple-string-iterator>)
    (inherit <iterator>)
    (fields (mutable   (index  <integer>)))
    (virtual-fields (immutable (subject <string>))
		    (mutable   (current <char>)))
    (protocol
     (lambda (make-iterator)
       (lambda (string)
	 (assert (string? string))
	 ((make-iterator string) 0))))
    (maker ()
	   (subject:	sentinel))
    (methods more? next))

  ;; accessor for the virtual field SUBJECT
  (define (<simple-string-iterator>-subject (I <iterator>))
    I.subject)

  ;; accessor and mutator for the virtual field CURRENT
  (define (<simple-string-iterator>-current (I <iterator>))
    I.current)
  (define (<simple-string-iterator>-current-set! (I <iterator>) value)
    (set! I.current value))

  ;;; virtual methods implementations

  (define-virtual-method <simple-string-iterator> (more? (I <simple-string-iterator>))
    (< I.index I.subject.length))

  (define-virtual-method <simple-string-iterator> (next (I <simple-string-iterator>))
    (if (< I.index I.subject.length)
	(begin0-let ((retval (getf (I.subject I.index))))
	  (set!  I.current retval)
	  (incr! I.index))
      (raise (make &stop-iteration I))))

;;; --------------------------------------------------------------------

  (let* (((S <string>) "ciao")
	 ((J <simple-string-iterator>)
	  (make <simple-string-iterator>
	    (subject: S)))
	 ((I <iterator>) J))

    (check (is-a? I <iterator>)		=> #t)
    (check (is-a? J <iterator>)		=> #t)
    (check (is-a? J <simple-string-iterator>) => #t)
    (check (sentinel? I.current)	=> #t)

    (check I.subject			=> "ciao")
    (check J.subject.length		=> 4)

    (check (I.more?)	=> #t)
    (check (I.next)	=> #\c)

    (check (I.more?)	=> #t)
    (check (I.next)	=> #\i)

    (check (I.more?)	=> #t)
    (check (I.next)	=> #\a)

    (check (I.more?)	=> #t)
    (check (I.next)	=> #\o)

    (check (I.more?)	=> #f)
    (check
    	(guard (E ((is-a? &stop-iteration)
    		   E.iterator)
    		  (else E))
    	  (I.next))
      => I)

    ;;once it is over, it is over forever
    (check (I.more?)	=> #f)
    (check
    	(guard (E ((is-a? &stop-iteration)
    		   E.iterator)
    		  (else E))
    	  (I.next))
      => I)

    #f)

  #t)


(parametrise ((check-test-name	'list))

  (let* (((L <list>)		'(c i a o))
	 ((I <iterator>)	(<list-iterator> ((subject: L)))))
    (check-for-true (is-a? I <list-iterator>))
    (check-for-true (is-a? I <iterator>))
    (check (I subject)	=> '(c i a o))
    (check-for-true (I more?))
    (check (I next)	=> 'c)
    (check-for-true (I more?))
    (check (I next)	=> 'i)
    (check-for-true (I more?))
    (check (I next)	=> 'a)
    (check (I current)	=> 'a)
    (check (I current)	=> 'a)
    (check-for-true (I more?))
    (check (I next)	=> 'o)
    (check-for-false (I more?))
    (check
    	(try
	    (I next)
	  (catch E
	    (&stop-iteration
	     (E iterator))
	    (else E)))
      => I)
    (check	;once it is over, it is over forever
    	(try
	    (I next)
	  (catch E
	    (&stop-iteration
	     (E iterator))
	    (else E)))
      => I)
    #f)

;;; --------------------------------------------------------------------

  ;; stride = 2
  (let* (((L <list>)		'(c i a o h e l L o))
	 ((I <iterator>)	(<list-iterator> ((subject: L)
						  (stride:  +2)))))
    (check-for-true (is-a? I <iterator>))
    (check-for-true (is-a? I <list-iterator>))
    (check (I subject)	=> '(c i a o h e l L o))
    (check-for-true (I more?))
    (check (I next)	=> 'c)
    (check-for-true (I more?))
    (check (I next)	=> 'a)
    (check-for-true (I more?))
    (check (I next)	=> 'h)
    (check (I current)	=> 'h)
    (check (I current)	=> 'h)
    (check-for-true (I more?))
    (check (I next)	=> 'l)
    (check-for-true (I more?))
    (check (I next)	=> 'o)
    (check-for-false (I more?))
    (check
    	(try
	    (I next)
	  (catch E
	    (&stop-iteration
	     (E iterator))
	    (else E)))
      => I)
    (check	;once it is over, it is over forever
    	(try
	    (I next)
	  (catch E
	    (&stop-iteration
	     (E iterator))
	    (else E)))
      => I)
    #f)

  ;; stride = 3
  (let* (((L <list>)		'(c i a o h e l L o))
	 ((I <iterator>)	(<list-iterator> ((subject: L)
						  (stride:  +3)))))
    (check-for-true (is-a? I <iterator>))
    (check-for-true (is-a? I <list-iterator>))
    (check (I subject)	=> '(c i a o h e l L o))
    ;;                       0     3     6
    (check-for-true (I more?))
    (check (I next)	=> 'c)
    (check-for-true (I more?))
    (check (I next)	=> 'o)
    (check-for-true (I more?))
    (check (I next)	=> 'l)
    (check (I current)	=> 'l)
    (check (I current)	=> 'l)
    (check-for-false (I more?))
    (check
    	(try
	    (I next)
	  (catch E
	    (&stop-iteration
	     (E iterator))
	    (else E)))
      => I)
    (check	;once it is over, it is over forever
    	(try
	    (I next)
	  (catch E
	    (&stop-iteration
	     (E iterator))
	    (else E)))
      => I)
    #f)

  #t)


(parametrise ((check-test-name	'string))

  ;;forwards string iteration
  (let* (((S <string>)		"ciao")
	 ((I <iterator>)	(<string-iterator> ((subject: S)
						    (stride:  1)))))
    (check-for-true (is-a? I <iterator>))
    (check-for-true (is-a? I <string-iterator>))
    (check-for-true (sentinel? (I current)))
    (check (I subject)	=> "ciao")
    (with-tags ((I <string-iterator>))
      (check (I subject length) => 4))
    (check-for-true (I more?))
    (check (I next)	=> #\c)
    (check (I next)	=> #\i)
    (check (I next)	=> #\a)
    (check (I next)	=> #\o)
    (check
    	(try
    	    (I next)
    	  (catch E
    	    (&stop-iteration
    	     (E iterator))
    	    (else E)))
      => I)
    (check	;once it is over, it is over forever
    	(try
    	    (I next)
    	  (catch E
    	    (&stop-iteration
    	     (E iterator))
    	    (else E)))
      => I)
    #f)

  ;;forwards string iteration, stride 2
  (let* (((S <string>)		"ciao")
	 ((I <iterator>)	(<string-iterator> ((subject: S)
						    (start:   0)
						    (past:    (S length))
						    (stride:  2)))))
    (check-for-true (is-a? I <iterator>))
    (check-for-true (is-a? I <string-iterator>))
    (check (I subject)	=> "ciao")
    (check (I next)	=> #\c)
    (check (I next)	=> #\a)
    (check (I current)	=> #\a)
    (check (I current)	=> #\a)
    (check
    	(try
	    (I next)
	  (catch E
	    (&stop-iteration
	     (E iterator))
	    (else E)))
      => I)
    (check	;once it is over, it is over forever
    	(try
	    (I next)
	  (catch E
	    (&stop-iteration
	     (E iterator))
	    (else E)))
      => I)
    #f)

  ;;backwards string iteration
  (let* (((S <string>)		"ciao")
	 ((I <iterator>)	(<string-iterator> ((subject: S)
						    (start:   (+ -1 (S length)))
						    (past:    0)
						    (stride:  -1)))))
    (check-for-true (is-a? I <iterator>))
    (check-for-true (is-a? I <string-iterator>))
    (check (I subject)	=> "ciao")
    (check (I next)	=> #\o)
    (check (I next)	=> #\a)
    (check (I next)	=> #\i)
    (check
    	(try
	    (I next)
	  (catch E
	    (&stop-iteration
	     (E iterator))
	    (else E)))
      => I)
    (check	;once it is over, it is over forever
    	(try
	    (I next)
	  (catch E
	    (&stop-iteration
	     (E iterator))
	    (else E)))
      => I)
    #f)

  #t)


(parametrise ((check-test-name	'vector))

  ;;forwards vector iteration
  (let* (((S <vector>)		'#(#\c #\i #\a #\o))
	 ((I <iterator>)	(<vector-iterator> ((subject: S)
						    (stride:  1)))))
    (check-for-true (is-a? I <iterator>))
    (check-for-true (is-a? I <vector-iterator>))
    (check-for-true (sentinel? (I current)))
    (check (I subject)	=> '#(#\c #\i #\a #\o))
    (with-tags ((I <vector-iterator>))
      (check (I subject length) => 4))
    (check-for-true (I more?))
    (check (I next)	=> #\c)
    (check (I next)	=> #\i)
    (check (I next)	=> #\a)
    (check (I next)	=> #\o)
    (check
    	(try
    	    (I next)
    	  (catch E
    	    (&stop-iteration
    	     (E iterator))
    	    (else E)))
      => I)
    (check	;once it is over, it is over forever
    	(try
    	    (I next)
    	  (catch E
    	    (&stop-iteration
    	     (E iterator))
    	    (else E)))
      => I)
    #f)

  ;;forwards vector iteration, stride 2
  (let* (((S <vector>)		'#(#\c #\i #\a #\o))
	 ((I <iterator>)	(<vector-iterator> ((subject: S)
						    (start:   0)
						    (past:    (S length))
						    (stride:  2)))))
    (check-for-true (is-a? I <iterator>))
    (check-for-true (is-a? I <vector-iterator>))
    (check (I subject)	=> '#(#\c #\i #\a #\o))
    (check (I next)	=> #\c)
    (check (I next)	=> #\a)
    (check (I current)	=> #\a)
    (check (I current)	=> #\a)
    (check
    	(try
	    (I next)
	  (catch E
	    (&stop-iteration
	     (E iterator))
	    (else E)))
      => I)
    (check	;once it is over, it is over forever
    	(try
	    (I next)
	  (catch E
	    (&stop-iteration
	     (E iterator))
	    (else E)))
      => I)
    #f)

  ;;backwards vector iteration
  (let* (((S <vector>)		'#(#\c #\i #\a #\o))
	 ((I <iterator>)	(<vector-iterator> ((subject: S)
						    (start:   (+ -1 (S length)))
						    (past:    0)
						    (stride:  -1)))))
    (check-for-true (is-a? I <iterator>))
    (check-for-true (is-a? I <vector-iterator>))
    (check (I subject)	=> '#(#\c #\i #\a #\o))
    (check (I next)	=> #\o)
    (check (I next)	=> #\a)
    (check (I next)	=> #\i)
    (check
    	(try
	    (I next)
	  (catch E
	    (&stop-iteration
	     (E iterator))
	    (else E)))
      => I)
    (check	;once it is over, it is over forever
    	(try
	    (I next)
	  (catch E
	    (&stop-iteration
	     (E iterator))
	    (else E)))
      => I)
    #f)

  #t)


(parametrise ((check-test-name	'bytevector))

  ;;forwards vector iteration
  (let* (((S <bytevector-u8>)	'#vu8(1 2 3 4))
	 ((I <iterator>)	(<bytevector-u8-iterator> ((subject: S)
							   (stride:  1)))))
    (check-for-true (is-a? I <iterator>))
    (check-for-true (is-a? I <bytevector-u8-iterator>))
    (check-for-true (sentinel? (I current)))
    (check (I subject)	=> '#vu8(1 2 3 4))
    (with-tags ((I <bytevector-u8-iterator>))
      (check (I subject length) => 4))
    (check-for-true (I more?))
    (check (I next)	=> 1)
    (check (I next)	=> 2)
    (check (I next)	=> 3)
    (check (I next)	=> 4)
    (check
    	(try
    	    (I next)
    	  (catch E
    	    (&stop-iteration
    	     (E iterator))
    	    (else E)))
      => I)
    (check	;once it is over, it is over forever
    	(try
    	    (I next)
    	  (catch E
    	    (&stop-iteration
    	     (E iterator))
    	    (else E)))
      => I)
    #f)

  ;;forwards vector iteration, stride 2
  (let* (((S <bytevector-u8>)	'#vu8(1 2 3 4))
	 ((I <iterator>)	(<bytevector-u8-iterator> ((subject: S)
							   (start:   0)
							   (past:    (S length))
							   (stride:  2)))))
    (check-for-true (is-a? I <iterator>))
    (check-for-true (is-a? I <bytevector-u8-iterator>))
    (check (I subject)	=> '#vu8(1 2 3 4))
    (check (I next)	=> 1)
    (check (I next)	=> 3)
    (check (I current)	=> 3)
    (check (I current)	=> 3)
    (check
    	(try
	    (I next)
	  (catch E
	    (&stop-iteration
	     (E iterator))
	    (else E)))
      => I)
    (check	;once it is over, it is over forever
    	(try
	    (I next)
	  (catch E
	    (&stop-iteration
	     (E iterator))
	    (else E)))
      => I)
    #f)

  ;;backwards vector iteration
  (let* (((S <bytevector-u8>)	'#vu8(1 2 3 4))
	 ((I <iterator>)	(<bytevector-u8-iterator> ((subject: S)
							   (start:   (+ -1 (S length)))
							   (past:    0)
							   (stride:  -1)))))
    (check-for-true (is-a? I <iterator>))
    (check-for-true (is-a? I <bytevector-u8-iterator>))
    (check (I subject)	=> '#vu8(1 2 3 4))
    (check (I next)	=> 4)
    (check (I next)	=> 3)
    (check (I next)	=> 2)
    (check
    	(try
	    (I next)
	  (catch E
	    (&stop-iteration
	     (E iterator))
	    (else E)))
      => I)
    (check	;once it is over, it is over forever
    	(try
	    (I next)
	  (catch E
	    (&stop-iteration
	     (E iterator))
	    (else E)))
      => I)
    #f)

  #t)


;;;; done

(check-report)

;;; end of file
