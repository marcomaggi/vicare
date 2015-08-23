;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the Scheme objects writer
;;;Date: Fri Aug 21, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(import (vicare)
  (only (vicare system $structs)
	base-rtd)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare features: Scheme objects writer\n")


;;;; helpers

(define (write-it obj)
  (receive (port extract)
      (open-string-output-port)
    (write obj port)
    (extract)))


(parametrise ((check-test-name	'strings)
	      (print-graph	#t))

  (check
      (write-it "ciao")
    => "\"ciao\"")

  (check
      (parametrise ((print-graph #f))
	(let ((str "ciao"))
	  (write-it (cons str str))))
    => "(\"ciao\" . \"ciao\")")

  (check
      (parametrise ((print-graph #t))
	(let ((str "ciao"))
	  (write-it (cons str str))))
    => "(#0=\"ciao\" . #0#)")

  #t)


(parametrise ((check-test-name	'gensyms)
	      (print-gensym	#f))

  (check
      (write-it (gensym "ciao"))
    => "ciao")

  (check
      (parametrise ((print-graph #f))
	(let ((sym (gensym "ciao")))
	  (write-it (cons sym sym))))
    => "(ciao . ciao)")

  (check
      (parametrise ((print-graph #t))
	(let ((sym (gensym "ciao")))
	  (write-it (cons sym sym))))
    => "(#0=ciao . #0#)")

  #t)


(parametrise ((check-test-name	'keywords))

  (check
      (write-it #:A)
    => "#:A")

  (check
      (write-it #:ciao)
    => "#:ciao")

;;; --------------------------------------------------------------------
;;; documentation examples

  (check
      (with-output-to-string
	(lambda ()
	  (display #:ciao)))
    => "#:ciao")

  #t)


(parametrise ((check-test-name	'vectors))

  (check
      (write-it '#(1 2 3))
    => "#(1 2 3)")

  (check
      (let ((vec (vector 1 2 3)))
	(vector-set! vec 1 vec)
	(write-it vec))
    => "#0=#(1 #0# 3)")

  #t)


(parametrise ((check-test-name	'structs)
	      (print-gensym	#f))

;;; struct instances, built-in printer

  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 2))
	(write-it A))
    => "#[struct duo one=1 two=2]")

  (check
      (parametrise ((print-graph #f))
	(internal-body
	  (define-struct duo	(one two))
	  (define A		(make-duo 1 2))
	  (define P		(cons A A))
	  (write-it P)))
    => "(#[struct duo one=1 two=2] . #[struct duo one=1 two=2])")

  (check
      (parametrise ((print-graph #t))
	(internal-body
	  (define-struct duo	(one two))
	  (define A		(make-duo 1 2))
	  (define P		(cons A A))
	  (write-it P)))
    => "(#0=#[struct duo one=1 two=2] . #0#)")

  ;;One struct with circular reference.
  ;;
  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 2))
	(set-duo-two! A A)
	(write-it A))
    => "#0=#[struct duo one=1 two=#0#]")

  ;;One struct with two circular references.
  ;;
  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 2))
	(set-duo-one! A A)
	(set-duo-two! A A)
	(write-it A))
    => "#0=#[struct duo one=#0# two=#0#]")

  ;;Two structs with circular reference.
  ;;
  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 2))
	(define B		(make-duo 3 A))
	(set-duo-two! A B)
	(write-it B))
    => "#0=#[struct duo one=3 two=#[struct duo one=1 two=#0#]]")

;;; --------------------------------------------------------------------
;;; custom printer

  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 2))
	(define (printer stru port sub-printer)
	  (fprintf port
		   "#{duo one=~s two=~s}"
		   (duo-one stru)
		   (duo-two stru)))
	(set-rtd-printer! (struct-type-descriptor duo) printer)
	(write-it A))
    => "#{duo one=1 two=2}")

;;; --------------------------------------------------------------------
;;; struct-type descriptors

  (check
      (write-it (base-rtd))
    => "#[struct-type base-rtd length=6 fields=(name length fields printer symbol destructor) printer=#f symbol=#f destructor=#f]")

  (check
      (internal-body
	(define-struct duo	(one two))
	(write-it (struct-type-descriptor duo)))
    => "#[struct-type duo length=2 fields=(one two) printer=#f symbol=duo destructor=#f]")

;;; --------------------------------------------------------------------
;;; documentation examples

  (parametrise ((print-graph #t))
    (internal-body
      (define-struct duo (one two))

      (check
	  (with-output-to-string
	    (lambda ()
	      (display (make-duo 1 2))))
	=> "#[struct duo one=1 two=2]")

      (check
	  (with-output-to-string
	    (lambda ()
	      (let* ((A (make-duo 1 2))
		     (B (make-duo A A)))
		(display B))))
	=> "#[struct duo one=#0=#[struct duo one=1 two=2] two=#0#]")

      (check
	  (with-output-to-string
	    (lambda ()
	      (let ((A (make-duo 1 (void))))
		(set-duo-two! A A)
		(display A))))
	=> "#0=#[struct duo one=1 two=#0#]")

      (void)))

  #t)


(parametrise ((check-test-name	'r6rs-records))

  (check
      (internal-body
	(define-record-type duo	(fields (mutable one) (mutable two)))
	(define A		(make-duo 1 2))
	(write-it A))
    => "#[r6rs-record duo one=1 two=2]")

  ;;One record with circular reference.
  ;;
  (check
      (internal-body
	(define-record-type duo	(fields (mutable one) (mutable two)))
	(define A		(make-duo 1 2))
	(duo-two-set! A A)
	(write-it A))
    => "#0=#[r6rs-record duo one=1 two=#0#]")

  ;;One record with two circular references.
  ;;
  (check
      (internal-body
	(define-record-type duo	(fields (mutable one) (mutable two)))
	(define A		(make-duo 1 2))
	(duo-one-set! A A)
	(duo-two-set! A A)
	(write-it A))
    => "#0=#[r6rs-record duo one=#0# two=#0#]")

  ;;Two records with circular reference.
  ;;
  (check
      (internal-body
	(define-record-type duo	(fields (mutable one) (mutable two)))
	(define A		(make-duo 1 2))
	(define B		(make-duo 3 A))
	(duo-two-set! A B)
	(write-it B))
    => "#0=#[r6rs-record duo one=3 two=#[r6rs-record duo one=1 two=#0#]]")

;;; --------------------------------------------------------------------
;;; record-type descriptors

  (check
      (internal-body
	(define-record-type duo	(fields one two))
	(write-it (record-type-descriptor duo)))
    => "#[rtd duo total-fields-number=2 fields-number=2 first-field-index=0 parent=#f sealed?=#f opaque?=#f uid=#f fields=#((#f . one) (#f . two)) initialiser=#<procedure> default-protocol=#f default-rcd=#f destructor=#f printer=#f]")

;;; --------------------------------------------------------------------
;;; record-constructor descriptors

  (check
      (internal-body
	(define-record-type duo	(fields one two))
	(write-it (record-constructor-descriptor duo)))
    => "#[rcd duo rtd=#[rtd duo total-fields-number=2 fields-number=2 first-field-index=0 parent=#f sealed?=#f opaque?=#f uid=#f fields=#((#f . one) (#f . two)) initialiser=#<procedure> default-protocol=#<procedure> default-rcd=#f destructor=#f printer=#f] parent-rcd=#f]")

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
