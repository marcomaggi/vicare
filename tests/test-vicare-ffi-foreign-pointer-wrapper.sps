;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for foreign pointer wrapper data structure
;;;Date: Sun Feb 24, 2013
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
  (prefix (vicare ffi)
	  ffi.)
  (prefix (vicare ffi foreign-pointer-wrapper)
	  ffi.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare FFI: foreign pointer wrapper data structure\n")


(parametrise ((check-test-name		'basic)
	      (struct-guardian-logger	#f))

  ;;Basic type definition:
  ;;
  ;;* No foreign destructor.
  ;;
  ;;* No collector struct.
  ;;
  ;;* No collected structs.
  ;;
  (ffi.define-struct-wrapping-foreign-pointer alpha
    (ffi.foreign-destructor #f)
    (ffi.collecting-type #f))

;;; --------------------------------------------------------------------
;;; owner constructor

  (check	;owner constructor
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/owner P)))
	(alpha? S))
    => #t)

  (check	;alive predicate
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/owner P)))
	(alpha?/alive S))
    => #t)

  (check	;unsafe alive predicate
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/owner P)))
	($live-alpha? S))
    => #t)

  (check	;owner constructor, getter
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/owner P)))
	(alpha-pointer-owner? S))
    => #t)

  (check	;owner constructor, unsafe getter
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/owner P)))
	($alpha-pointer-owner? S))
    => #t)

;;; --------------------------------------------------------------------
;;; not-owner constructor

  (check	;not owner constructor
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/not-owner P)))
	(alpha? S))
    => #t)

  (check	;alive predicate
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/not-owner P)))
	(alpha?/alive S))
    => #t)

  (check	;unsafe alive predicate
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/not-owner P)))
	($live-alpha? S))
    => #t)

  (check	;not-owner constructor, getter
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/not-owner P)))
	(alpha-pointer-owner? S))
    => #f)

  (check	;not-owner constructor, unsafe getter
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/not-owner P)))
	($alpha-pointer-owner? S))
    => #f)

;;; --------------------------------------------------------------------
;;; unsafe destructor

  (check	;unsafe destructor invocation
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/owner P)))
	($destroy-alpha S))
    => (void))

  (check	;unsafe destructor invocation, twice
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/owner P)))
	($destroy-alpha S)
	($destroy-alpha S))
    => (void))

  (check	;unsafe destructor invocation, dead struct safe pred
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/owner P)))
	($destroy-alpha S)
	(alpha?/alive S))
    => #f)

  (check	;unsafe destructor invocation, dead struct unsafe pred
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/owner P)))
	($destroy-alpha S)
	($live-alpha? S))
    => #f)

;;; --------------------------------------------------------------------
;;; custom destructor

  (check	;custom destructor invocation
      (with-result
       (let* ((P (integer->pointer 123))
	      (S (make-alpha/owner P))
	      (D (lambda (S)
		   (add-result (pointer->integer (alpha-pointer S))))))
	 (set-alpha-custom-destructor! S D)
	 ($destroy-alpha S)))
    => `(,(void) (123)))

  (check	;custom  destructor   invocation,  invoking   twice  the
		;destructor
      (with-result
       (let* ((P (integer->pointer 123))
	      (S (make-alpha/owner P))
	      (D (lambda (S)
		   (add-result (pointer->integer (alpha-pointer S))))))
	 (set-alpha-custom-destructor! S D)
	 ($destroy-alpha S)
	 ($destroy-alpha S)))
    => `(,(void) (123)))

;;; --------------------------------------------------------------------
;;; UID gensym

  (check	;safe getter
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/owner P)))
	(symbol? (alpha-uid S)))
    => #t)

  (check	;unsafe getter
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/owner P)))
	(symbol? ($alpha-uid S)))
    => #t)

  (collect))


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'ffi.define-struct-wrapping-foreign-pointer 'scheme-indent-function 1)
;; End:
