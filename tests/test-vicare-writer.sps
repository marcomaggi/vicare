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

(define (display-it obj)
  (receive (port extract)
      (open-string-output-port)
    (display obj port)
    (extract)))

(define (pretty-it obj)
  (receive (port extract)
      (open-string-output-port)
    (pretty-print obj port)
    (extract)))

(define-syntax check-pretty-string
  (syntax-rules ()
    ((_ ?body)
     (check
	 (string? (receive-and-return (str)
		      (pretty-it ?body)
		    (display str (current-error-port))
		    (newline (current-error-port))))
       => #t))
    ))


(parametrise ((check-test-name	'strings-writer)
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


(parametrise ((check-test-name	'strings-pretty)
	      (print-graph	#t))

  (check
      (pretty-it "ciao")
    => "\"ciao\"\n")

  (check
      (parametrise ((print-graph #f))
	(let ((str "ciao"))
	  (pretty-it (cons str str))))
    => "(\"ciao\" . \"ciao\")\n")

  (check
      (parametrise ((print-graph #t))
	(let ((str "ciao"))
	  (pretty-it (cons str str))))
    => "(#0=\"ciao\" . #0#)\n")

  #t)


(parametrise ((check-test-name	'gensyms-writer)
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


(parametrise ((check-test-name	'gensyms-pretty)
	      (print-gensym	#f))

  (check
      (pretty-it (gensym "ciao"))
    => "ciao\n")

  (check
      (parametrise ((print-graph #f))
	(let ((sym (gensym "ciao")))
	  (pretty-it (cons sym sym))))
    => "(ciao . ciao)\n")

  ;;Shared gensym in pair.
  ;;
  (check
      (parametrise ((print-graph #t))
	(let ((sym (gensym "ciao")))
	  (pretty-it (cons sym sym))))
    => "(#0=ciao . #0#)\n")

  ;;Shared gensym in list.
  ;;
  (check
      (parametrise ((print-graph #t))
	(let ((sym (gensym "ciao")))
	  (pretty-it (list sym sym))))
    => "(#0=ciao #0#)\n")

  #t)


(parametrise ((check-test-name	'keywords-writer))

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


(parametrise ((check-test-name	'keywords-pretty))

  (check
      (pretty-it #:A)
    => "#:A\n")

  (check
      (pretty-it #:ciao)
    => "#:ciao\n")

  #t)


(parametrise ((check-test-name	'vectors-writer))

  (check
      (write-it '#())
    => "#()")

  (check
      (write-it '#(1 2 3))
    => "#(1 2 3)")

  ;;Shared object.
  ;;
  (check
      (let* ((vec1 (vector 1 2 3))
	     (vec2 (vector vec1 vec1)))
	(write-it vec2))
    => "#(#0=#(1 2 3) #0#)")

  ;;Cyclic reference.
  ;;
  (check
      (let ((vec (vector 1 2 3)))
	(vector-set! vec 1 vec)
	(write-it vec))
    => "#0=#(1 #0# 3)")

;;; --------------------------------------------------------------------

  (check
      (display-it '#())
    => "#()")

  (check
      (display-it '#(1 2 3))
    => "#(1 2 3)")

  ;;Shared object.
  ;;
  (check
      (let* ((vec1 (vector 1 2 3))
	     (vec2 (vector vec1 vec1)))
	(display-it vec2))
    => "#(#0=#(1 2 3) #0#)")

  ;;Cyclic reference.
  ;;
  (check
      (let ((vec (vector 1 2 3)))
	(vector-set! vec 1 vec)
	(display-it vec))
    => "#0=#(1 #0# 3)")

  #t)


(parametrise ((check-test-name	'vectors-pretty))

  (check
      (pretty-it '#())
    => "#()\n")

  (check
      (pretty-it '#(1 2 3))
    => "#(1 2 3)\n")

  ;;Shared object.
  ;;
  (check
      (let* ((vec1 (vector 1 2 3))
	     (vec2 (vector vec1 vec1)))
	(pretty-it vec2))
    => "#(#0=#(1 2 3) #0#)\n")

  ;;Cyclic reference.
  ;;
  (check
      (let ((vec (vector 1 2 3)))
	(vector-set! vec 1 vec)
	(pretty-it vec))
    => "#0=#(1 #0# 3)\n")

  #t)


(parametrise ((check-test-name	'pairs-writer)
	      (print-graph	#t))

  (check
      (write-it '(1 . 2))
    => "(1 . 2)")

  ;;Shared object.
  ;;
  (check
      (let* ((ls1 (cons 1 2))
	     (ls2 (cons ls1 ls1)))
	(write-it ls2))
    => "(#0=(1 . 2) . #0#)")

  ;;Cyclic reference.
  ;;
  (check
      (let ((ls (cons 1 (void))))
	(set-cdr! ls ls)
	(write-it ls))
    => "#0=(1 . #0#)")

;;; --------------------------------------------------------------------

  (check
      (display-it '(1 . 2))
    => "(1 . 2)")

  #t)


(parametrise ((check-test-name	'pairs-pretty)
	      (print-graph	#t))

  (check
      (pretty-it '(1 . 2))
    => "(1 . 2)\n")

  #t)


(parametrise ((check-test-name	'lists-writer)
	      (print-graph	#t))

  (check
      (write-it '())
    => "()")

  (check
      (write-it '(1 2))
    => "(1 2)")

  ;;Shared object.
  ;;
  (check
      (let* ((ls1 (list 1 2 3))
	     (ls2 (list ls1 ls1)))
	(write-it ls2))
    => "(#0=(1 2 3) #0#)")

  ;;Cyclic reference.
  ;;
  (check
      (let ((ls (list 1 (void) 3)))
	(set-car! (cdr ls) ls)
	(write-it ls))
    => "#0=(1 #0# 3)")

;;; --------------------------------------------------------------------

  (check
      (display-it '())
    => "()")

  (check
      (display-it '(1 2))
    => "(1 2)")

  #t)


(parametrise ((check-test-name	'lists-pretty)
	      (print-graph	#t))

  (check
      (pretty-it '())
    => "()\n")

  (check
      (pretty-it '(1))
    => "(1)\n")

  #t)


(parametrise ((check-test-name	'structs-writer)
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

  ;;No use of sub-printer.
  ;;
  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 2))
	(define (printer stru port sub-printer)
	  (fprintf port
		   "#{duo one=~s two=~s}"
		   (duo-one stru)
		   (duo-two stru)))
	(set-struct-type-printer! (struct-type-descriptor duo) printer)
	(write-it A))
    => "#{duo one=1 two=2}")

  ;;Use of sub-printer on immediate objects.
  ;;
  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 2))
	(define (printer stru port sub-printer)
	  (display "#{duo " port)
	  (sub-printer (duo-one stru))
	  (display " " port)
	  (sub-printer (duo-two stru))
	  (display "}" port))
	(set-struct-type-printer! (struct-type-descriptor duo) printer)
	(write-it A))
    => "#{duo 1 2}")

  ;;Use of sub-printer on immediate objects.
  ;;
  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 2))
	(define (printer stru port sub-printer)
	  (display "#{duo one=" port)
	  (sub-printer (duo-one stru))
	  (display " two=" port)
	  (sub-printer (duo-two stru))
	  (display "}" port))
	(set-struct-type-printer! (struct-type-descriptor duo) printer)
	(write-it A))
    => "#{duo one=1 two=2}")

  ;;Use of sub-printer on compound objects.
  ;;
  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 2))
	(define B		(make-duo 3 A))
	(define (printer stru port sub-printer)
	  (display "#{duo one=" port)
	  (sub-printer (duo-one stru))
	  (display " two=" port)
	  (sub-printer (duo-two stru))
	  (display "}" port))
	(set-struct-type-printer! (struct-type-descriptor duo) printer)
	(write-it B))
    => "#{duo one=3 two=#{duo one=1 two=2}}")

  ;;Use of sub-printer on shared objects objects.
  ;;
  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 2))
	(define B		(make-duo A A))
	(define (printer stru port sub-printer)
	  (display "#{duo one=" port)
	  (sub-printer (duo-one stru))
	  (display " two=" port)
	  (sub-printer (duo-two stru))
	  (display "}" port))
	(set-struct-type-printer! (struct-type-descriptor duo) printer)
	(write-it B))
    => "#{duo one=#0=#{duo one=1 two=2} two=#0#}")

  ;;Use of sub-printer on cyclic references.
  ;;
  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 (void)))
	(define (printer stru port sub-printer)
	  (display "#{duo one=" port)
	  (sub-printer (duo-one stru))
	  (display " two=" port)
	  (sub-printer (duo-two stru))
	  (display "}" port))
	(set-struct-type-printer! (struct-type-descriptor duo) printer)
	(set-duo-two! A A)
	(write-it A))
    => "#0=#{duo one=1 two=#0#}")

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

;;;

  (internal-body

    (define-struct duo
      (one two))

    (set-struct-type-printer! (struct-type-descriptor duo)
			      (lambda (stru port sub-printer)
				(display "#{duo " port)
				(sub-printer (duo-one stru))
				(display " " port)
				(sub-printer (duo-two stru))
				(display "}" port)))

    ;; simple struct
    (check
	(with-output-to-string
	  (lambda ()
	    (display (make-duo 1 2))))
      => "#{duo 1 2}")

    ;; struct with shared object
    (check
	(with-output-to-string
	  (lambda ()
	    (let* ((A (make-duo 1 2))
		   (B (make-duo A A)))
	      (display B))))
      => "#{duo #0=#{duo 1 2} #0#}")

    ;; struct with cyclic reference to itself
    (check
	(with-output-to-string
	  (lambda ()
	    (let ((A (make-duo 1 (void))))
	      (set-duo-two! A A)
	      (display A))))
      => "#0=#{duo 1 #0#}")

    (void))

  #t)


(parametrise ((check-test-name	'structs-pretty)
	      (print-gensym	#f))

;;; struct instances, built-in printer

  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 2))
	(pretty-it A))
    => "#[struct duo one=1 two=2]\n")

  (check
      (parametrise ((print-graph #f))
	(internal-body
	  (define-struct duo	(one two))
	  (define A		(make-duo 1 2))
	  (define P		(cons A A))
	  (pretty-it P)))
    => "(#[struct duo one=1 two=2] . #[struct duo one=1 two=2])\n")

  (check
      (parametrise ((print-graph #t))
	(internal-body
	  (define-struct duo	(one two))
	  (define A		(make-duo 1 2))
	  (define P		(cons A A))
	  (pretty-it P)))
    => "(#0=#[struct duo one=1 two=2] . #0#)\n")

  ;;One struct with circular reference.
  ;;
  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 2))
	(set-duo-two! A A)
	(pretty-it A))
    => "#0=#[struct duo one=1 two=#0#]\n")

  ;;One struct with two circular references.
  ;;
  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 2))
	(set-duo-one! A A)
	(set-duo-two! A A)
	(pretty-it A))
    => "#0=#[struct duo one=#0# two=#0#]\n")

  ;;Two structs with circular reference.
  ;;
  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 2))
	(define B		(make-duo 3 A))
	(set-duo-two! A B)
	(pretty-it B))
    => "#0=#[struct duo one=3 two=#[struct duo one=1 two=#0#]]\n")

;;; --------------------------------------------------------------------
;;; custom printer

  ;;No use of sub-printer.
  ;;
  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 2))
	(define (printer stru port sub-printer)
	  (fprintf port
		   "#{duo one=~s two=~s}"
		   (duo-one stru)
		   (duo-two stru)))
	(set-struct-type-printer! (struct-type-descriptor duo) printer)
	(pretty-it A))
    => "#{duo one=1 two=2}\n")

  ;;Use of sub-printer on immediate objects.
  ;;
  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 2))
	(define (printer stru port sub-printer)
	  (display "#{duo " port)
	  (sub-printer (duo-one stru))
	  (display " " port)
	  (sub-printer (duo-two stru))
	  (display "}" port))
	(set-struct-type-printer! (struct-type-descriptor duo) printer)
	(pretty-it A))
    => "#{duo 1 2}\n")

  ;;Use of sub-printer on immediate objects.
  ;;
  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 2))
	(define (printer stru port sub-printer)
	  (display "#{duo one=" port)
	  (sub-printer (duo-one stru))
	  (display " two=" port)
	  (sub-printer (duo-two stru))
	  (display "}" port))
	(set-struct-type-printer! (struct-type-descriptor duo) printer)
	(pretty-it A))
    => "#{duo one=1 two=2}\n")

  ;;Use of sub-printer on compound objects.
  ;;
  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 2))
	(define B		(make-duo 3 A))
	(define (printer stru port sub-printer)
	  (display "#{duo one=" port)
	  (sub-printer (duo-one stru))
	  (display " two=" port)
	  (sub-printer (duo-two stru))
	  (display "}" port))
	(set-struct-type-printer! (struct-type-descriptor duo) printer)
	(pretty-it B))
    => "#{duo one=3 two=#{duo one=1 two=2}}\n")

  ;;Use of sub-printer on shared objects objects.
  ;;
  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 2))
	(define B		(make-duo A A))
	(define (printer stru port sub-printer)
	  (display "#{duo one=" port)
	  (sub-printer (duo-one stru))
	  (display " two=" port)
	  (sub-printer (duo-two stru))
	  (display "}" port))
	(set-struct-type-printer! (struct-type-descriptor duo) printer)
	(pretty-it B))
    => "#{duo one=#0=#{duo one=1 two=2} two=#0#}\n")

  ;;Use of sub-printer on cyclic references.
  ;;
  (check
      (internal-body
	(define-struct duo	(one two))
	(define A		(make-duo 1 (void)))
	(define (printer stru port sub-printer)
	  (display "#{duo one=" port)
	  (sub-printer (duo-one stru))
	  (display " two=" port)
	  (sub-printer (duo-two stru))
	  (display "}" port))
	(set-struct-type-printer! (struct-type-descriptor duo) printer)
	(set-duo-two! A A)
	(pretty-it A))
    => "#0=#{duo one=1 two=#0#}\n")

;;; --------------------------------------------------------------------
;;; struct-type descriptors

  (check-pretty-string (base-rtd))

  (check-pretty-string
   (internal-body
     (define-struct duo	(one two))
     (struct-type-descriptor duo)))

  #t)


(parametrise ((check-test-name	'records-writer))

  (check
      (internal-body
	(define-record-type duo	(fields (mutable one) (mutable two)))
	(define A		(make-duo 1 2))
	(write-it A))
    => "#[record duo one=1 two=2]")

  ;;One record with circular reference.
  ;;
  (check
      (internal-body
	(define-record-type duo	(fields (mutable one) (mutable two)))
	(define A		(make-duo 1 2))
	(duo-two-set! A A)
	(write-it A))
    => "#0=#[record duo one=1 two=#0#]")

  ;;One record with two circular references.
  ;;
  (check
      (internal-body
	(define-record-type duo	(fields (mutable one) (mutable two)))
	(define A		(make-duo 1 2))
	(duo-one-set! A A)
	(duo-two-set! A A)
	(write-it A))
    => "#0=#[record duo one=#0# two=#0#]")

  ;;Two records with circular reference.
  ;;
  (check
      (internal-body
	(define-record-type duo	(fields (mutable one) (mutable two)))
	(define A		(make-duo 1 2))
	(define B		(make-duo 3 A))
	(duo-two-set! A B)
	(write-it B))
    => "#0=#[record duo one=3 two=#[record duo one=1 two=#0#]]")

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

;;; --------------------------------------------------------------------
;;; documentation examples

  ;;Built-in printer.
  ;;
  (internal-body

    (define-record-type duo
      (fields (mutable one)
	      (mutable two)))

    ;; simple record
    (check
	(with-output-to-string
	  (lambda ()
	    (display (make-duo 1 2))))
      => "#[record duo one=1 two=2]")

    ;; record with shared object
    (check
	(with-output-to-string
	  (lambda ()
	    (let* ((A (make-duo 1 2))
		   (B (make-duo A A)))
	      (display B))))
      => "#[record duo one=#0=#[record duo one=1 two=2] two=#0#]")

    ;; record with cyclic reference to itself
    (check
	(with-output-to-string
	  (lambda ()
	    (let ((A (make-duo 1 (void))))
	      (duo-two-set! A A)
	      (display A))))
      => "#0=#[record duo one=1 two=#0#]")

    (void))

;;;

  ;;Custom printer.
  ;;
  (internal-body

    (define-record-type duo
      (fields (mutable one)
	      (mutable two)))

    (record-type-printer-set! (record-type-descriptor duo)
			      (lambda (stru port sub-printer)
				(display "#@{duo " port)
				(sub-printer (duo-one stru))
				(display " " port)
				(sub-printer (duo-two stru))
				(display "@}" port)))

    ;; simple record
    (check
	(with-output-to-string
	  (lambda ()
	    (display (make-duo 1 2))))
      => "#@{duo 1 2@}")

    ;; record with shared object
    (check
	(with-output-to-string
	  (lambda ()
	    (let* ((A (make-duo 1 2))
		   (B (make-duo A A)))
	      (display B))))
      => "#@{duo #0=#@{duo 1 2@} #0#@}")

    ;; record with cyclic reference to itself
    (check
	(with-output-to-string
	  (lambda ()
	    (let ((A (make-duo 1 (void))))
	      (duo-two-set! A A)
	      (display A))))
      => "#0=#@{duo 1 #0#@}")

    (void))

  #t)


(parametrise ((check-test-name	'records-pretty)
	      (print-gensym	#f))

;;; record instances, built-in printer

  (check
      (internal-body
	(define-record-type duo
	  (fields (mutable one) (mutable two)))
	(define A
	  (make-duo 1 2))
	(pretty-it A))
    => "#[record duo one=1 two=2]\n")

  (check
      (parametrise ((print-graph #f))
	(internal-body
	  (define-record-type duo
	    (fields (mutable one) (mutable two)))
	  (define A	(make-duo 1 2))
	  (define P	(cons A A))
	  (pretty-it P)))
    => "(#[record duo one=1 two=2] . #[record duo one=1 two=2])\n")

  (check
      (parametrise ((print-graph #t))
	(internal-body
	  (define-record-type duo
	    (fields (mutable one) (mutable two)))
	  (define A	(make-duo 1 2))
	  (define P	(cons A A))
	  (pretty-it P)))
    => "(#0=#[record duo one=1 two=2] . #0#)\n")

  ;;One record with circular reference.
  ;;
  (check
      (internal-body
	(define-record-type duo
	  (fields (mutable one) (mutable two)))
	(define A	(make-duo 1 2))
	(duo-two-set! A A)
	(pretty-it A))
    => "#0=#[record duo one=1 two=#0#]\n")

  ;;One record with two circular references.
  ;;
  (check
      (internal-body
	(define-record-type duo
	  (fields (mutable one) (mutable two)))
	(define A
	  (make-duo 1 2))
	(duo-one-set! A A)
	(duo-two-set! A A)
	(pretty-it A))
    => "#0=#[record duo one=#0# two=#0#]\n")

  ;;Two records with circular reference.
  ;;
  (check
      (internal-body
	(define-record-type duo
	  (fields (mutable one) (mutable two)))
	(define A	(make-duo 1 2))
	(define B	(make-duo 3 A))
	(duo-two-set! A B)
	(pretty-it B))
    => "#0=#[record duo one=3 two=#[record duo one=1 two=#0#]]\n")

;;; --------------------------------------------------------------------
;;; custom printer

  ;;No use of sub-printer.
  ;;
  (check
      (internal-body
	(define-record-type duo
	  (fields (mutable one) (mutable two)))
	(define A
	  (make-duo 1 2))
	(define (printer stru port sub-printer)
	  (fprintf port
		   "#{duo one=~s two=~s}"
		   (duo-one stru)
		   (duo-two stru)))
	(record-type-printer-set! (record-type-descriptor duo) printer)
	(pretty-it A))
    => "#{duo one=1 two=2}\n")

  ;;Use of sub-printer on immediate objects.
  ;;
  (check
      (internal-body
	(define-record-type duo
	  (fields (mutable one) (mutable two)))
	(define A		(make-duo 1 2))
	(define (printer stru port sub-printer)
	  (display "#{duo " port)
	  (sub-printer (duo-one stru))
	  (display " " port)
	  (sub-printer (duo-two stru))
	  (display "}" port))
	(record-type-printer-set! (record-type-descriptor duo) printer)
	(pretty-it A))
    => "#{duo 1 2}\n")

  ;;Use of sub-printer on immediate objects.
  ;;
  (check
      (internal-body
	(define-record-type duo
	  (fields (mutable one) (mutable two)))
	(define A		(make-duo 1 2))
	(define (printer stru port sub-printer)
	  (display "#{duo one=" port)
	  (sub-printer (duo-one stru))
	  (display " two=" port)
	  (sub-printer (duo-two stru))
	  (display "}" port))
	(record-type-printer-set! (record-type-descriptor duo) printer)
	(pretty-it A))
    => "#{duo one=1 two=2}\n")

  ;;Use of sub-printer on compound objects.
  ;;
  (check
      (internal-body
	(define-record-type duo
	  (fields (mutable one) (mutable two)))
	(define A		(make-duo 1 2))
	(define B		(make-duo 3 A))
	(define (printer stru port sub-printer)
	  (display "#{duo one=" port)
	  (sub-printer (duo-one stru))
	  (display " two=" port)
	  (sub-printer (duo-two stru))
	  (display "}" port))
	(record-type-printer-set! (record-type-descriptor duo) printer)
	(pretty-it B))
    => "#{duo one=3 two=#{duo one=1 two=2}}\n")

  ;;Use of sub-printer on shared objects objects.
  ;;
  (check
      (internal-body
	(define-record-type duo
	  (fields (mutable one) (mutable two)))
	(define A		(make-duo 1 2))
	(define B		(make-duo A A))
	(define (printer stru port sub-printer)
	  (display "#{duo one=" port)
	  (sub-printer (duo-one stru))
	  (display " two=" port)
	  (sub-printer (duo-two stru))
	  (display "}" port))
	(record-type-printer-set! (record-type-descriptor duo) printer)
	(pretty-it B))
    => "#{duo one=#0=#{duo one=1 two=2} two=#0#}\n")

  ;;Use of sub-printer on cyclic references.
  ;;
  (check
      (internal-body
	(define-record-type duo
	  (fields (mutable one) (mutable two)))
	(define A		(make-duo 1 (void)))
	(define (printer stru port sub-printer)
	  (display "#{duo one=" port)
	  (sub-printer (duo-one stru))
	  (display " two=" port)
	  (sub-printer (duo-two stru))
	  (display "}" port))
	(record-type-printer-set! (record-type-descriptor duo) printer)
	(duo-two-set! A A)
	(pretty-it A))
    => "#0=#{duo one=1 two=#0#}\n")

;;; --------------------------------------------------------------------
;;; record-type descriptors

  (check-pretty-string
   (internal-body
     (define-record-type duo
       (fields (mutable one) (mutable two)))
     (record-type-descriptor duo)))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
