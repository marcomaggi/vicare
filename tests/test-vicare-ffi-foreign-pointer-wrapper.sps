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
  (vicare arguments validation)
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
  (ffi.define-foreign-pointer-wrapper alpha
    (ffi.foreign-destructor #f)
    (ffi.collector-struct-type #f))

  ;; Printer check.
  (when #f
    (check-pretty-print (make-alpha/owner (integer->pointer 123))))

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
	($alpha-alive? S))
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
	($alpha-alive? S))
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
	($alpha-finalise S))
    => (void))

  (check	;unsafe destructor invocation, twice
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/owner P)))
	($alpha-finalise S)
	($alpha-finalise S))
    => (void))

  (check	;unsafe destructor invocation, dead struct safe pred
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/owner P)))
	($alpha-finalise S)
	(alpha?/alive S))
    => #f)

  (check	;unsafe destructor invocation, dead struct unsafe pred
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/owner P)))
	($alpha-finalise S)
	($alpha-alive? S))
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
	 ($alpha-finalise S)))
    => `(,(void) (123)))

  (check	;custom  destructor   invocation,  invoking   twice  the
		;destructor
      (with-result
       (let* ((P (integer->pointer 123))
	      (S (make-alpha/owner P))
	      (D (lambda (S)
		   (add-result (pointer->integer (alpha-pointer S))))))
	 (set-alpha-custom-destructor! S D)
	 ($alpha-finalise S)
	 ($alpha-finalise S)))
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

;;; --------------------------------------------------------------------
;;; argument validators

  (check	;struct validator, success
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/owner P))
	     (who 'test))
	(with-arguments-validation (who)
	    ((alpha		S))
	  #t))
    => #t)

  (check	;struct validator, failure
      (guard (E ((assertion-violation? E)
		 #t)
		(else E))
	(let ((S 123)
	      (who 'test))
	  (with-arguments-validation (who)
	      ((alpha		S))
	    #t)))
    => #t)

  (check	;alive struct validator
      (let* ((P (integer->pointer 123))
	     (S (make-alpha/owner P))
	     (who 'test))
	(with-arguments-validation (who)
	    ((alpha/alive	S))
	  #t))
    => #t)

  (check	;alive struct validator, failure
      (guard (E ((assertion-violation? E)
		 #t)
		(else E))
	(let ((S 123)
	      (who 'test))
	  (with-arguments-validation (who)
	      ((alpha/alive	S))
	    #f)))
    => #t)

  (check	;alive struct validator, not alive failure
      (guard (E ((assertion-violation? E)
		 #t)
		(else E))
	(let* ((P (integer->pointer 123))
	       (S (make-alpha/owner P))
	       (who 'test))
	  ($alpha-finalise S)
	  (with-arguments-validation (who)
	      ((alpha/alive	S))
	    #t)))
    => #t)

  (collect))


(parametrise ((check-test-name		'destructor)
	      (struct-guardian-logger	#f))

  ;;Type definition:
  ;;
  ;;* With foreign destructor.
  ;;
  ;;* No collector struct.
  ;;
  ;;* No collected structs.
  ;;
  (ffi.define-foreign-pointer-wrapper alpha
    (ffi.foreign-destructor foreign-alpha-destructor)
    (ffi.collector-struct-type #f))

  (define verbose? #f)

  (define (foreign-alpha-destructor S)
    (when (or #f verbose?)
      (check-pretty-print (list 'enter-foreign-destructor S)))
    (ffi.free ($alpha-pointer S))
    (when (or #f verbose?)
      (check-pretty-print (list 'leave-foreign-destructor S))))

  ;; Printer check.
  (when (or #f verbose?)
    (check-pretty-print (make-alpha/owner (ffi.malloc* 1024))))

;;; --------------------------------------------------------------------
;;; owner constructor

  (check	;owner constructor
      (let* ((P (ffi.malloc* 1024))
	     (S (make-alpha/owner P)))
	(alpha? S))
    => #t)

  (check	;alive predicate
      (let* ((P (ffi.malloc* 1024))
	     (S (make-alpha/owner P)))
	(alpha?/alive S))
    => #t)

  (check	;unsafe alive predicate
      (let* ((P (ffi.malloc* 1024))
	     (S (make-alpha/owner P)))
	($alpha-alive? S))
    => #t)

  (check	;owner constructor, getter
      (let* ((P (ffi.malloc* 1024))
	     (S (make-alpha/owner P)))
	(alpha-pointer-owner? S))
    => #t)

  (check	;owner constructor, unsafe getter
      (let* ((P (ffi.malloc* 1024))
	     (S (make-alpha/owner P)))
	($alpha-pointer-owner? S))
    => #t)

;;; --------------------------------------------------------------------
;;; not-owner constructor

  (check	;not owner constructor
      (let* ((P (ffi.malloc* 1024))
	     (S (make-alpha/not-owner P)))
	(alpha? S))
    => #t)

  (check	;alive predicate
      (let* ((P (ffi.malloc* 1024))
	     (S (make-alpha/not-owner P)))
	(alpha?/alive S))
    => #t)

  (check	;unsafe alive predicate
      (let* ((P (ffi.malloc* 1024))
	     (S (make-alpha/not-owner P)))
	($alpha-alive? S))
    => #t)

  (check	;not-owner constructor, getter
      (let* ((P (ffi.malloc* 1024))
	     (S (make-alpha/not-owner P)))
	(alpha-pointer-owner? S))
    => #f)

  (check	;not-owner constructor, unsafe getter
      (let* ((P (ffi.malloc* 1024))
	     (S (make-alpha/not-owner P)))
	($alpha-pointer-owner? S))
    => #f)

;;; --------------------------------------------------------------------
;;; unsafe destructor, owner struct

  (check	;unsafe destructor invocation
      (let* ((P (ffi.malloc* 1024))
	     (S (make-alpha/owner P)))
	($alpha-finalise S))
    => (void))

  (check	;unsafe destructor invocation, twice
      (let* ((P (ffi.malloc* 1024))
	     (S (make-alpha/owner P)))
	($alpha-finalise S)
	($alpha-finalise S))
    => (void))

  (check	;unsafe destructor invocation, dead struct safe pred
      (let* ((P (ffi.malloc* 1024))
	     (S (make-alpha/owner P)))
	($alpha-finalise S)
	(alpha?/alive S))
    => #f)

  (check	;unsafe destructor invocation, dead struct unsafe pred
      (let* ((P (ffi.malloc* 1024))
	     (S (make-alpha/owner P)))
	($alpha-finalise S)
	($alpha-alive? S))
    => #f)

;;; --------------------------------------------------------------------
;;; unsafe destructor, not owner struct

  (check	;unsafe destructor invocation
      (let* ((P (ffi.malloc* 1024))
	     (S (make-alpha/not-owner P)))
	($alpha-finalise S)
	(list (alpha?/alive S)
	      ($alpha-alive? S)
	      (ffi.pointer-null? P)))
    => '(#f #f #f))

  (check	;owner struct finalised before not owner struct
      (let* ((P (ffi.malloc* 1024))
	     (Q (ffi.pointer-clone P))
	     (S (make-alpha/owner P))
	     (R (make-alpha/not-owner Q)))
	($alpha-finalise S)
	(list (alpha?/alive S)
	      ($alpha-alive? S)
	      (ffi.pointer-null? P)
	      (alpha?/alive R)
	      ($alpha-alive? R)
	      (ffi.pointer-null? Q)))
    => '(#f #f #t #t #t #f))

;;; --------------------------------------------------------------------
;;; custom destructor

  (check	;custom destructor invocation
      (with-result
       (let* ((P (ffi.malloc* 1024))
	      (S (make-alpha/owner P))
	      (D (lambda (S)
		   (add-result (pointer? (alpha-pointer S))))))
	 (set-alpha-custom-destructor! S D)
	 ($alpha-finalise S)))
    => `(,(void) (#t)))

  (check	;custom  destructor   invocation,  invoking   twice  the
		;destructor
      (with-result
       (let* ((P (ffi.malloc* 1024))
	      (S (make-alpha/owner P))
	      (D (lambda (S)
		   (add-result (pointer? (alpha-pointer S))))))
	 (set-alpha-custom-destructor! S D)
	 ($alpha-finalise S)
	 ($alpha-finalise S)))
    => `(,(void) (#t)))

;;; --------------------------------------------------------------------
;;; UID gensym

  (check	;safe getter
      (let* ((P (ffi.malloc* 1024))
	     (S (make-alpha/owner P)))
	(symbol? (alpha-uid S)))
    => #t)

  (check	;unsafe getter
      (let* ((P (ffi.malloc* 1024))
	     (S (make-alpha/owner P)))
	(symbol? ($alpha-uid S)))
    => #t)

;;; --------------------------------------------------------------------
;;; argument validators

  (check	;struct validator, success
      (let* ((P (ffi.malloc* 1024))
	     (S (make-alpha/owner P))
	     (who 'test))
	(with-arguments-validation (who)
	    ((alpha		S))
	  #t))
    => #t)

  (check	;struct validator, failure
      (guard (E ((assertion-violation? E)
		 #t)
		(else E))
	(let ((S 123)
	      (who 'test))
	  (with-arguments-validation (who)
	      ((alpha		S))
	    #t)))
    => #t)

  (check	;alive struct validator
      (let* ((P (ffi.malloc* 1024))
	     (S (make-alpha/owner P))
	     (who 'test))
	(with-arguments-validation (who)
	    ((alpha/alive	S))
	  #t))
    => #t)

  (check	;alive struct validator, failure
      (guard (E ((assertion-violation? E)
		 #t)
		(else E))
	(let ((S 123)
	      (who 'test))
	  (with-arguments-validation (who)
	      ((alpha/alive	S))
	    #f)))
    => #t)

  (check	;alive struct validator, not alive failure
      (guard (E ((assertion-violation? E)
		 #t)
		(else E))
	(let* ((P (ffi.malloc* 1024))
	       (S (make-alpha/owner P))
	       (who 'test))
	  ($alpha-finalise S)
	  (with-arguments-validation (who)
	      ((alpha/alive	S))
	    #t)))
    => #t)

  (collect))


(parametrise ((check-test-name		'collector)
	      (struct-guardian-logger	#f))

  ;;Type definition:
  ;;
  ;;* No foreign destructor.
  ;;
  ;;* No collector struct.
  ;;
  ;;* With collected structs.
  ;;
  (ffi.define-foreign-pointer-wrapper alpha
    (ffi.foreign-destructor #f)
    (ffi.collector-struct-type #f)
    (ffi.collected-struct-type beta))

  ;;Type definition:
  ;;
  ;;* No foreign destructor.
  ;;
  ;;* With collector struct.
  ;;
  ;;* No collected structs.
  ;;
  (ffi.define-foreign-pointer-wrapper beta
    (ffi.foreign-destructor #f)
    (ffi.collector-struct-type alpha))

  (define verbose? #f)

;;; --------------------------------------------------------------------

  (check	;check that collector and collected reference each other
      (let* ((P (integer->pointer 123))
	     (Q (integer->pointer 456))
	     (A (make-alpha/owner P))
	     (B (make-beta/owner Q A)))
	(list (eq? A ($beta-collector-alpha B))
	      (let ((V ($alpha-vector-of-collected-beta A)))
		(and (vector? V)
		     (= 1 (vector-length V))
		     (eq? B (vector-ref V 0))))))
    => '(#t #t))

  (check	;after the finalisation of  the collected, the collector
		;is collecting nothing
      (let* ((P (integer->pointer 123))
	     (Q (integer->pointer 456))
	     (A (make-alpha/owner P))
	     (B (make-beta/owner Q A)))
	($beta-finalise B)
	($alpha-vector-of-collected-beta A))
    => '#())

  (check	;after the finalisation of  the collector, the collected
		;is finalised too
      (let* ((P (integer->pointer 123))
	     (Q (integer->pointer 456))
	     (A (make-alpha/owner P))
	     (B (make-beta/owner Q A)))
	($alpha-finalise A)
	(list ($alpha-vector-of-collected-beta A)
	      (alpha?/alive A)
	      (beta?/alive B)))
    => '(#() #f #f))

;;; --------------------------------------------------------------------

  (check	;collected without collector
      (let* ((Q (integer->pointer 456))
	     (B (make-beta/owner Q #f)))
	($beta-finalise B)
	(beta?/alive B))
    => #f)

  (collect))


(parametrise ((check-test-name		'double-collector)
	      (struct-guardian-logger	#f))

  ;;Type definition:
  ;;
  ;;* No foreign destructor.
  ;;
  ;;* No collector struct.
  ;;
  ;;* With collected structs.
  ;;
  (ffi.define-foreign-pointer-wrapper alpha
    (ffi.foreign-destructor #f)
    (ffi.collector-struct-type #f)
    (ffi.collected-struct-type beta)
    (ffi.collected-struct-type gamma))

  ;;Type definition:
  ;;
  ;;* No foreign destructor.
  ;;
  ;;* With collector struct.
  ;;
  ;;* No collected structs.
  ;;
  (ffi.define-foreign-pointer-wrapper beta
    (ffi.foreign-destructor #f)
    (ffi.collector-struct-type alpha))

  ;;Type definition:
  ;;
  ;;* No foreign destructor.
  ;;
  ;;* With collector struct.
  ;;
  ;;* No collected structs.
  ;;
  (ffi.define-foreign-pointer-wrapper gamma
    (ffi.foreign-destructor #f)
    (ffi.collector-struct-type alpha))

  (define verbose? #f)

;;; --------------------------------------------------------------------

  (check	;check that collector and collected reference each other
      (let* ((P (integer->pointer 123))
	     (Q (integer->pointer 456))
	     (R (integer->pointer 789))
	     (A (make-alpha/owner P))
	     (B (make-beta/owner  Q A))
	     (G (make-gamma/owner R A)))
	(list (eq? A ($beta-collector-alpha B))
	      (eq? A ($beta-collector-alpha G))
	      (let ((VB ($alpha-vector-of-collected-beta  A))
		    (TB ($alpha-table-of-collected-beta   A)))
		(and (vector? VB)
		     (= 1 (vector-length VB))
		     (eq? B (vector-ref VB 0))
		     (eq? B (hashtable-ref TB (beta-uid B) #f))))
	      (let ((VG ($alpha-vector-of-collected-gamma A))
		    (TG ($alpha-table-of-collected-gamma  A)))
		(and (vector? VG)
		     (= 1 (vector-length VG))
		     (eq? G (vector-ref VG 0))
		     (eq? G (hashtable-ref TG (gamma-uid G) #f))))))
    => '(#t #t #t #t))

  (check	;after  the finalisation  of one  of the  collected, the
		;collector is collecting only the other collected
      (let* ((P (integer->pointer 123))
	     (Q (integer->pointer 456))
	     (R (integer->pointer 789))
	     (A (make-alpha/owner P))
	     (B (make-beta/owner  Q A))
	     (G (make-gamma/owner R A)))
	($beta-finalise B)
	(list ($alpha-vector-of-collected-beta A)
	      (eq? G (vector-ref ($alpha-vector-of-collected-gamma A) 0))))
    => '(#() #t))

  (check	;after  the finalisation  of one  of the  collected, the
		;collector is collecting only the other collected
      (let* ((P (integer->pointer 123))
	     (Q (integer->pointer 456))
	     (R (integer->pointer 789))
	     (A (make-alpha/owner P))
	     (B (make-beta/owner  Q A))
	     (G (make-gamma/owner R A)))
	($gamma-finalise G)
	(list ($alpha-vector-of-collected-gamma A)
	      (eq? B (vector-ref ($alpha-vector-of-collected-beta A) 0))))
    => '(#() #t))

  (check	;after  the  finalisation  of both  the  collected,  the
		;collector is collecting nothing
      (let* ((P (integer->pointer 123))
	     (Q (integer->pointer 456))
	     (R (integer->pointer 789))
	     (A (make-alpha/owner P))
	     (B (make-beta/owner  Q A))
	     (G (make-gamma/owner R A)))
	($beta-finalise  B)
	($gamma-finalise G)
	(list ($alpha-vector-of-collected-gamma A)
	      ($alpha-vector-of-collected-beta  A)))
    => '(#() #()))

  (check	;after the finalisation of  the collector, the collected
 		;are finalised too
      (let* ((P (integer->pointer 123))
	     (Q (integer->pointer 456))
	     (R (integer->pointer 789))
	     (A (make-alpha/owner P))
	     (B (make-beta/owner  Q A))
	     (G (make-gamma/owner R A)))
	($alpha-finalise A)
	(list ($alpha-vector-of-collected-gamma A)
	      ($alpha-vector-of-collected-beta  A)
	      (alpha?/alive A)
	      (beta?/alive  B)
	      (gamma?/alive G)))
    => '(#() #() #f #f #f))

  (collect))


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'ffi.define-foreign-pointer-wrapper 'scheme-indent-function 1)
;; End:
