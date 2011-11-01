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
    (vicare syntactic-extensions)
    (prefix (vicare unsafe-operations)
	    unsafe.)
    (ikarus system $structs))


;;;; helpers

(define (%field-is-a-symbol? x)
  (unless (symbol? x)
    (assertion-violation 'make-struct-type "not a valid field name" x)))

(define-argument-validation (name who name)
  (string? name)
  (assertion-violation who "expected string as name argument" name))

(define-argument-validation (list-of-fields who fields)
  (list? fields)
  (assertion-violation who "fields must be a list" fields))

(define-argument-validation (rtd who rtd)
  (rtd? rtd)
  (assertion-violation who "expected structure rtd as argument" rtd))

(define-argument-validation (struct-of-type who struct rtd)
  (and ($struct? struct)
       (eq? rtd ($struct-rtd struct)))
  (assertion-violation who "not a data structure of correct type" struct rtd))

(define-argument-validation (struct who x)
  ($struct? x)
  (assertion-violation who "expected data structure as argument" x))

(define-argument-validation (index who index struct)
  (and (fixnum? index)
       (unsafe.fx>= index 0)
       (unsafe.fx<  index (rtd-length ($struct-rtd struct))))
  (assertion-violation who
    "expected fixnum in range for structure field as index argument" index struct))

(define-argument-validation (printer who printer)
  (procedure? printer)
  (assertion-violation who "expected procedure as printer argument" printer))


;;;; low level RTD operations

(define-inline (make-rtd name fields printer symbol)
  ($struct (base-rtd) name (length fields) fields printer symbol))

(define-inline (rtd? x)
  (and ($struct? x)
       (eq? ($struct-rtd x) (base-rtd))))


;;;; unsafe RTD fields accessors

(define-inline (rtd-name rtd)
  ($struct-ref rtd 0))

(define-inline (rtd-length rtd)
  ($struct-ref rtd 1))

(define-inline (rtd-fields rtd)
  ($struct-ref rtd 2))

(define-inline (rtd-printer rtd)
  ($struct-ref rtd 3))

(define-inline (rtd-symbol rtd)
  ($struct-ref rtd 4))


;;;; unsafe RTD fields mutators

(define-inline (set-rtd-name! rtd name)
  ($struct-set! rtd 0 name))

(define-inline (set-rtd-length! rtd n)
  ($struct-set! rtd 1 n))

(define-inline (set-rtd-fields! rtd fields)
  ($struct-set! rtd 2 fields))

(define-inline ($set-rtd-printer! rtd printer)
  ($struct-set! rtd 3 printer))

(define-inline (set-rtd-symbol! rtd symbol)
  ($struct-set! rtd 4 symbol))


;;;; structure type descriptor

(define make-struct-type
  ;;Build and  return a new structure  type descriptor.  NAME  must be a
  ;;string representing the type name.  FIELDS must be a list of symbols
  ;;representing the  field names.
  ;;
  ;;The optional UID argument must be a symbol uniquely identifying this
  ;;type; when  not supplied, a symbol is  automatically generated.  The
  ;;$SYMBOL-VALUE field of  UID is set to the RTD; if  UID already has a
  ;;symbol value,  such value must be  a struct descriptor  equal to the
  ;;newly created RTD.
  ;;
  (case-lambda
   ((name fields)
    (define who 'make-struct-type)
    (with-arguments-validation (who)
	((name		 name)
	 (list-of-fields fields))
      (for-each %field-is-a-symbol? fields)
      (let* ((uid (gensym name))
	     (rtd (make-rtd name fields #f uid)))
	(set-symbol-value! uid rtd)
	rtd)))
   ((name fields uid)
    (define who 'make-struct-type)
    (with-arguments-validation (who)
	((name		 name)
	 (list-of-fields fields))
      (for-each %field-is-a-symbol? fields)
      (if (symbol-bound? uid)
	  (let ((rtd (symbol-value uid)))
	    (unless (and (string=? name (struct-type-name rtd))
			 (equal? fields (struct-type-field-names rtd)))
	      (assertion-violation who "mismatching data structure definition"))
	    rtd)
	(let ((rtd (make-rtd name fields #f uid)))
	  (set-symbol-value! uid rtd)
	  rtd))))))

(define (struct-type-name rtd)
  ;;Return a string represnting the name of structures of type RTD.
  ;;
  (define who 'struct-type-name)
  (with-arguments-validation (who)
      ((rtd rtd))
    (rtd-name rtd)))

(define (struct-type-symbol rtd)
  ;;Return a symbol uniquely identifying the data structure type RTD.
  ;;
  (define who 'struct-type-symbol)
  (with-arguments-validation (who)
      ((rtd rtd))
    (rtd-symbol rtd)))

(define (struct-type-field-names rtd)
  ;;Return  a  list of  symbols  representing  the  names of  fields  in
  ;;structures of type RTD.
  ;;
  (define who 'struct-type-field-names)
  (with-arguments-validation (who)
      ((rtd rtd))
    (rtd-fields rtd)))

(define (set-rtd-printer! rtd printer)
  ;;Select the procedure PRINTER as  printer for data structures of type
  ;;RTD.   The printer  accepts  as  3 arguments:  the  structure to  be
  ;;printed,  the port  to  which  write a  string  represention of  the
  ;;structure with DISPLAY,  a function to be optionally  applied to the
  ;;field values to print them.
  ;;
  (define who set-rtd-printer!)
  (with-arguments-validation (who)
      ((rtd	rtd)
       (printer	printer))
    ($set-rtd-printer! rtd printer)))


;;;; data structure functions

(define (struct-constructor rtd)
  ;;Return a constructor function for  data structures of type RTD.  The
  ;;constructor accepts as  many arguments as fields defined  by RTD and
  ;;returns a new structure instance.
  ;;
  (define who 'struct-constructor)
  (define (%set-fields r f* i n)
    (cond ((null? f*)
	   (if (unsafe.fx= i n)
	       r
	     #f))
	  ((unsafe.fx< i n)
	   (if (null? f*)
	       #f
	     (begin
	       ($struct-set! r i (unsafe.car f*))
	       (%set-fields r (unsafe.cdr f*) (unsafe.fxadd1 i) n))))
	  (else #f)))
  (with-arguments-validation (who)
      ((rtd rtd))
    (lambda args
      (let* ((n (rtd-length rtd))
	     (r ($make-struct rtd n)))
	(or (%set-fields r args 0 n)
	    (assertion-violation who
	      "incorrect number of arguments to the constructor" rtd))))))

(define (struct-predicate rtd)
  ;;Return a predicate function for structures of type RTD.
  ;;
  (define who 'struct-predicate)
  (with-arguments-validation (who)
      ((rtd rtd))
    (lambda (x)
      (and ($struct? x)
	   (eq? rtd ($struct-rtd x))))))

(define (%field-index i rtd who)
  (cond ((fixnum? i)
	 (unless (and (unsafe.fx>= i 0) (unsafe.fx< i (rtd-length rtd)))
	   (assertion-violation who "out of range for rtd" i rtd))
	 i)
	((symbol? i)
	 (letrec ((lookup (lambda (n ls)
			    (cond ((null? ls)
				   (assertion-violation who "not a field" rtd))
				  ((eq? i (unsafe.car ls))
				   n)
				  (else
				   (lookup (unsafe.fx+ n 1) (unsafe.cdr ls)))))))
	   (lookup 0 (rtd-fields rtd))))
	(else
	 (assertion-violation who "not a valid index" i))))

(define (struct-field-accessor rtd i)
  ;;Return  an  accessor function  for  the field  at  index  I of  data
  ;;structures of type RTD.
  ;;
  (define who 'struct-field-accessor)
  (with-arguments-validation (who)
      ((rtd rtd))
    (let ((i (%field-index i rtd who)))
      (lambda (x)
	(with-arguments-validation (who)
	    ((struct-of-type x rtd))
	  ($struct-ref x i))))))

(define (struct-field-mutator rtd i)
  ;;Return  a  mutator  function  for  the  field at  index  I  of  data
  ;;structures of type RTD.
  ;;
  (define who 'struct-field-mutator)
  (with-arguments-validation (who)
      ((rtd rtd))
    (let ((i (%field-index i rtd 'struct-field-mutator)))
      (lambda (x v)
	(with-arguments-validation (who)
	    ((struct-of-type x rtd))
	  ($struct-set! x i v))))))


;;;; data structure inspection

(define (struct? x . rest)
  ;;(struct? x)
  ;;(struct? x rtd)
  ;;
  ;;When no RTD argument is given  return true if X is a data structure.
  ;;When RTD is given: return true if X is a data structure and it is of
  ;;type RTD.
  ;;
  (define who 'struct?)
  (if (null? rest)
      ($struct? x)
    (let ((rtd (unsafe.car rest)))
      (unless (null? (unsafe.cdr rest))
	(assertion-violation who "too many arguments"))
      (unless (rtd? rtd)
	(assertion-violation who "not an rtd"))
      (and ($struct? x)
	   (eq? rtd ($struct-rtd x))))))

(define (struct-rtd x)
  ;;Return the RTD of the data structure X.
  ;;
  (define who 'struct-rtd)
  (with-arguments-validation (who)
      ((struct x))
    ($struct-rtd x)))

(define (struct-length x)
  ;;Return the number of fields in the data structure X.
  ;;
  (define who 'struct-length)
  (with-arguments-validation (who)
      ((struct x))
    (rtd-length ($struct-rtd x))))

(define (struct-name x)
  ;;Return a string representing the name of the data structure X.
  ;;
  (define who 'struct-name)
  (with-arguments-validation (who)
      ((struct x))
    (rtd-name ($struct-rtd x))))

(define (struct-printer x)
  ;;Return  the  procedure  being  the  printer function  for  the  data
  ;;structure X.
  ;;
  (define who 'struct-printer)
  (with-arguments-validation (who)
      ((struct x))
    (rtd-printer ($struct-rtd x))))

(define (struct-ref x i)
  ;;Return the value of field at index I in the data structure X.
  ;;
  (define who 'struct-ref)
  (with-arguments-validation (who)
      ((struct	x)
       (index	i x))
    ($struct-ref x i)))

(define (struct-set! x i v)
  ;;Store V in the field at index I in the data structure X.
  ;;
  (define who 'struct-set!)
  (with-arguments-validation (who)
      ((struct	x)
       (index	i x))
    ($struct-set! x i v)))


;;;; done

;;Initialise the fields of the base RTD.
(set-rtd-fields! (base-rtd) '(name fields length printer symbol))
(set-rtd-name! (base-rtd) "base-rtd")
($set-rtd-printer! (base-rtd)
		   (lambda (rtd port wr)
		     (define who 'struct-type-printer)
		     (with-arguments-validation (who)
			 ((rtd rtd))
		       (display "#<" port)
		       (display (rtd-name rtd) port)
		       (display " rtd>" port))))

#| end of libray (ikarus structs) |# )


(library (ikarus systems structs)
  (export $struct-ref $struct/rtd?)
  (import (ikarus))
  (define $struct-ref struct-ref)
  (define ($struct/rtd? x rtd)
    (import (ikarus system $structs))
    ($struct/rtd? x rtd)))

;;; end of file
