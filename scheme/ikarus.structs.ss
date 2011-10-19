;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.

(library (ikarus structs)
  (export
    ;; struct type descriptor constructor
    make-struct-type

    ;; struct type descriptor inspection
    struct-type-name		struct-type-symbol
    struct-type-field-names

    ;; struct type descriptor customisation
    set-rtd-printer!

    ;; struct constructor and predicate
    struct?
    struct-constructor		struct-predicate

    ;; struct accessors and mutators
    struct-ref			struct-set!
    struct-field-accessor	struct-field-mutator

    ;; structure inspection
    struct-rtd			(rename (struct-rtd struct-type-descriptor))
    struct-name			struct-printer
    struct-length)
  (import (except (ikarus)
		  ;; struct type descriptor constructor
		  make-struct-type

		  ;; struct type descriptor inspection
		  struct-type-name	struct-type-symbol
		  struct-type-field-names

		  ;; struct type descriptor customisation
		  set-rtd-printer!

		  ;; struct accessors and mutators
		  struct?
		  struct-constructor	struct-predicate

		  ;; struct accessors and mutators
		  struct-ref		struct-set!
		  struct-field-accessor	struct-field-mutator

		  ;; structure inspection
		  struct-rtd		struct-type-descriptor
		  struct-name		struct-printer
		  struct-length)
    (ikarus system $structs)
    (ikarus system $pairs)
    (ikarus system $fx))


;;;; syntax helpers

(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ... . ?rest) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ... . ?rest)
	  (begin ?form0 ?form ...)))))))



;;;; unsafe RTD fields accessors

(define (rtd-name rtd)
  ($struct-ref rtd 0))

(define (rtd-length rtd)
  ($struct-ref rtd 1))

(define (rtd-fields rtd)
  ($struct-ref rtd 2))

(define (rtd-printer rtd)
  ($struct-ref rtd 3))

(define (rtd-symbol rtd)
  ($struct-ref rtd 4))


;;;; unsafe RTD fields mutators

(define (set-rtd-name! rtd name)
  ($struct-set! rtd 0 name))

(define (set-rtd-length! rtd n)
  ($struct-set! rtd 1 n))

(define (set-rtd-fields! rtd fields)
  ($struct-set! rtd 2 fields))

(define ($set-rtd-printer! rtd printer)
  ($struct-set! rtd 3 printer))

(define (set-rtd-symbol! rtd symbol)
  ($struct-set! rtd 4 symbol))


;;;; helpers

(define (%field-is-a-symbol? x)
  (unless (symbol? x)
    (assertion-violation 'make-struct-type "not a valid field name" x)))

(define-inline (%assert-argument-is-name who name)
  (unless (string? name)
    (assertion-violation who "expected string as name argument" name)))

(define-inline (%assert-argument-is-list-of-fields who fields)
  (unless (list? fields)
    (assertion-violation who "fields must be a list" fields)))

(define-inline (%assert-argument-is-rtd who rtd)
  (unless (rtd? rtd)
    (assertion-violation who "expected structure rtd as argument" rtd)))

(define-inline (%assert-argument-is-struct-of-type who struct rtd)
  (unless (and ($struct? struct)
	       (eq? rtd ($struct-rtd struct)))
    (assertion-violation who "not a data structure of correct type" struct rtd)))

(define-inline (%assert-argument-is-struct who x)
  (unless ($struct? x)
    (assertion-violation who "expected data structure as argument" x)))

(define-inline (%assert-argument-is-index who index struct)
  (unless (fixnum? index)
    (assertion-violation who "expected fixnum as structure field index" index))
  (let ((n (rtd-length ($struct-rtd struct))))
    (unless (and ($fx>= index 0)
		 ($fx<  index n))
      (assertion-violation who "structure field index is out of range" index struct))))

(define-inline (%assert-argument-is-printer who printer)
  (unless (procedure? printer)
    (assertion-violation who "expected procedure as printer argument" printer)))


(define (make-rtd name fields printer symbol)
  ($struct (base-rtd) name (length fields) fields printer symbol))

(define (rtd? x)
  (and ($struct? x)
       (eq? ($struct-rtd x) (base-rtd))))

(define (set-fields r f* i n)
  (cond ((null? f*)
	 (if ($fx= i n)
	     r
	   #f))
	(($fx< i n)
	 (if (null? f*)
	     #f
	   (begin
	     ($struct-set! r i ($car f*))
	     (set-fields r ($cdr f*) ($fxadd1 i) n))))
	(else #f)))

(define make-struct-type
  (case-lambda
   ((name fields)
    (define who 'make-struct-type)
    (%assert-argument-is-name who name)
    (%assert-argument-is-list-of-fields who fields)
    (for-each %field-is-a-symbol? fields)
    (let* ((g   (gensym name))
	   (rtd (make-rtd name fields #f g)))
      (set-symbol-value! g rtd)
      rtd))
   ((name fields g)
    (define who 'make-struct-type)
    (%assert-argument-is-name who name)
    (%assert-argument-is-list-of-fields who fields)
    (for-each %field-is-a-symbol? fields)
    (if (symbol-bound? g)
	(let ((rtd (symbol-value g)))
	  (unless (and (string=? name (struct-type-name rtd))
		       (equal? fields (struct-type-field-names rtd)))
	    (assertion-violation who
	      "mismatching data structure definition"))
	  rtd)
      (let ((rtd (make-rtd name fields #f g)))
	(set-symbol-value! g rtd)
	rtd)))))

(define (struct-type-name rtd)
  (define who 'struct-type-name)
  (%assert-argument-is-rtd who rtd)
  (rtd-name rtd))

(define (struct-type-symbol rtd)
  (define who 'struct-type-symbol)
  (%assert-argument-is-rtd who rtd)
  (rtd-symbol rtd))

(define (struct-type-field-names rtd)
  (define who 'struct-type-field-names)
  (%assert-argument-is-rtd who rtd)
  (rtd-fields rtd))

(define (struct-constructor rtd)
  (define who 'struct-constructor)
  (%assert-argument-is-rtd who rtd)
  (lambda args
    (let* ((n (rtd-length rtd))
	   (r ($make-struct rtd n)))
      (or (set-fields r args 0 n)
	  (assertion-violation who
	    "incorrect number of arguments to the constructor" rtd)))))

(define (struct-predicate rtd)
  (define who 'struct-predicate)
  (%assert-argument-is-rtd who rtd)
  (lambda (x)
    (and ($struct? x)
	 (eq? ($struct-rtd x) rtd))))

(define (%field-index i rtd who)
  (cond ((fixnum? i)
	 (unless (and ($fx>= i 0) ($fx< i (rtd-length rtd)))
	   (assertion-violation who "out of range for rtd" i rtd))
	 i)
	((symbol? i)
	 (letrec ((lookup (lambda (n ls)
			    (cond ((null? ls)
				   (assertion-violation who "not a field" rtd))
				  ((eq? i ($car ls))
				   n)
				  (else
				   (lookup ($fx+ n 1) ($cdr ls)))))))
	   (lookup 0 (rtd-fields rtd))))
	(else
	 (assertion-violation who "not a valid index" i))))

(define (struct-field-accessor rtd i)
  (define who 'struct-field-accessor)
  (%assert-argument-is-rtd who rtd)
  (let ((i (%field-index i rtd who)))
    (lambda (x)
      (%assert-argument-is-struct-of-type who x rtd)
      ($struct-ref x i))))

(define (struct-field-mutator rtd i)
  (define who 'struct-field-mutator)
  (%assert-argument-is-rtd who rtd)
  (let ((i (%field-index i rtd 'struct-field-mutator)))
    (lambda (x v)
      (%assert-argument-is-struct-of-type who x rtd)
      ($struct-set! x i v))))

(define (struct? x . rest)
  ;; struct? x
  ;; struct? x rtd
  ;;
  ;;When no RTD argument is given  return true if X is a data structure.
  ;;When RTD is given: return true if X is a data structure and it is of
  ;;type RTD.
  ;;
  (define who 'struct?)
  (if (null? rest)
      ($struct? x)
    (let ((rtd ($car rest)))
      (unless (null? ($cdr rest))
	(assertion-violation who "too many arguments"))
      (unless (rtd? rtd)
	(assertion-violation who "not an rtd"))
      (and ($struct? x)
	   (eq? rtd ($struct-rtd x))))))

(define (struct-rtd x)
  (define who 'struct-rtd)
  (%assert-argument-is-struct who x)
  ($struct-rtd x))

(define (struct-length x)
  (define who 'struct-length)
  (%assert-argument-is-struct who x)
  (rtd-length ($struct-rtd x)))

(define (struct-name x)
  (define who 'struct-name)
  (%assert-argument-is-struct who x)
  (rtd-name ($struct-rtd x)))

(define (struct-printer x)
  (define who 'struct-printer)
  (%assert-argument-is-struct who x)
  (rtd-printer ($struct-rtd x)))

(define (struct-ref x i)
  (define who 'struct-ref)
  (%assert-argument-is-struct who x)
  (%assert-argument-is-index who i x)
  ($struct-ref x i))

(define (struct-set! x i v)
  (define who 'struct-set!)
  (%assert-argument-is-struct who x)
  (%assert-argument-is-index who i x)
  ($struct-set! x i v))

(define (set-rtd-printer! rtd printer)
  (define who set-rtd-printer!)
  (%assert-argument-is-rtd who rtd)
  (%assert-argument-is-printer who printer)
  ($set-rtd-printer! rtd printer))


;;;; done

;;Initialise the fields of the base RTD.
(set-rtd-fields! (base-rtd) '(name fields length printer symbol))
(set-rtd-name! (base-rtd) "base-rtd")
($set-rtd-printer! (base-rtd)
		   (lambda (x p wr)
		     (unless (rtd? x)
		       (assertion-violation 'struct-type-printer "not an rtd"))
		     (display "#<" p)
		     (display (rtd-name x) p)
		     (display " rtd>" p)))

)


(library (ikarus systems structs)
  (export $struct-ref $struct/rtd?)
  (import (ikarus))
  (define $struct-ref struct-ref)
  (define ($struct/rtd? x rtd)
    (import (ikarus system $structs))
    ($struct/rtd? x rtd)))

;;; end of file
