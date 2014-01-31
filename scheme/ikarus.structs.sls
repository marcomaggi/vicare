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
    struct-type-descriptor?
    struct-type-name		struct-type-symbol
    struct-type-field-names	struct-type-destructor

    ;; struct type descriptor customisation
    default-struct-printer
    set-rtd-printer!		set-rtd-destructor!

    ;; struct constructor and predicate
    struct?
    struct-constructor		struct-predicate
    struct=?

    ;; struct accessors and mutators
    struct-ref			struct-set!
    struct-field-accessor	struct-field-mutator
    struct-reset

    ;;FIXME This  renaming must be removed  in future (at the  next boot
    ;;rotation?) and the identifier  STRUCT-TYPE-DESCRIPTOR bound to the
    ;;syntax.  (Marco Maggi; Fri Jan 31, 2014)
    (rename (struct-rtd struct-type-descriptor))

    ;; structure inspection
    struct-rtd
    struct-name			struct-printer
    struct-destructor		struct-length)
  (import (except (ikarus)
		  ;; struct type descriptor constructor
		  make-struct-type

		  ;; struct type descriptor inspection
		  struct-type-descriptor?
		  struct-type-name		struct-type-symbol
		  struct-type-field-names	struct-type-destructor

		  ;; struct type descriptor customisation
		  default-struct-printer
		  set-rtd-printer!		set-rtd-destructor!

		  ;; struct accessors and mutators
		  struct?			struct=?
		  struct-constructor		struct-predicate

		  ;; struct accessors and mutators
		  struct-ref			struct-set!
		  struct-field-accessor		struct-field-mutator
		  struct-reset

		  ;; structure inspection
		  struct-rtd
		  struct-name			struct-printer
		  struct-destructor		struct-length)
    (ikarus.emergency)
    (vicare language-extensions syntaxes)
    (vicare unsafe operations)
    (ikarus system $structs))


;;;; helpers

(define-argument-validation (name who name)
  (string? name)
  (procedure-argument-violation who "expected string as name argument" name))

(define-argument-validation (list-of-fields who fields)
  (list? fields)
  (procedure-argument-violation who "fields must be a list" fields))

(define-argument-validation (rtd who rtd)
  (struct-type-descriptor? rtd)
  (procedure-argument-violation who "expected structure rtd as argument" rtd))

(define-argument-validation (struct-of-type who struct rtd)
  (and ($struct? struct)
       (eq? rtd ($struct-rtd struct)))
  (procedure-argument-violation who "not a data structure of correct type" struct rtd))

(define-argument-validation (struct who x)
  ($struct? x)
  (procedure-argument-violation who "expected data structure as argument" x))

(define-argument-validation (index who index struct)
  (and (fixnum? index)
       ($fx>= index 0)
       ($fx<  index ($std-length ($struct-rtd struct))))
  (procedure-argument-violation who
    "expected fixnum in range for structure field as index argument" index struct))

(define-argument-validation (printer who printer)
  (procedure? printer)
  (procedure-argument-violation who "expected procedure as printer argument" printer))

(define-argument-validation (destructor who destructor)
  (procedure? destructor)
  (procedure-argument-violation who "expected procedure as destructor argument" destructor))


;;;; low level RTD operations

(define-inline ($make-rtd name fields uid)
  ($struct (base-rtd) name (length fields) fields
	   default-struct-printer uid #f))

;;; --------------------------------------------------------------------
;;; unsafe RTD fields accessors

(define-inline ($std-name std)
  ($struct-ref std 0))

(define-inline ($std-length std)
  ($struct-ref std 1))

(define-inline ($std-fields std)
  ($struct-ref std 2))

(define-inline ($std-printer std)
  ($struct-ref std 3))

(define-inline ($std-symbol std)
  ($struct-ref std 4))

(define-inline ($std-destructor std)
  ($struct-ref std 5))

;;; --------------------------------------------------------------------
;;; unsafe STD fields mutators

(define-inline ($set-std-name! std name)
  ($struct-set! std 0 name))

(define-inline ($set-std-length! std n)
  ($struct-set! std 1 n))

(define-inline ($set-std-fields! std fields)
  ($struct-set! std 2 fields))

(define-inline ($set-std-printer! std printer)
  ($struct-set! std 3 printer))

(define-inline ($set-std-symbol! std symbol)
  ($struct-set! std 4 symbol))

(define-inline ($set-std-destructor! std destructor-func)
  ($struct-set! std 5 destructor-func))


;;;; structure type descriptor

(module (make-struct-type)
  ;;Build and  return a new structure  type descriptor.  NAME must  be a
  ;;string representing the type name.  FIELDS must be a list of symbols
  ;;representing the field names.
  ;;
  ;;The optional UID argument must be a symbol uniquely identifying this
  ;;type; when not  supplied, a symbol is  automatically generated.  The
  ;;$SYMBOL-VALUE field of UID  is set to the RTD; if  UID already has a
  ;;symbol value,  such value must be  a struct descriptor equal  to the
  ;;newly created RTD.
  ;;
  (case-define* make-struct-type
    ((name fields)
     (with-arguments-validation (__who__)
	 ((name		 name)
	  (list-of-fields fields))
       (for-each %field-is-a-symbol? fields)
       (let* ((uid (gensym name))
	      (rtd ($make-rtd name fields uid)))
	 (set-symbol-value! uid rtd)
	 rtd)))

    ((name fields uid)
     (with-arguments-validation (__who__)
	 ((name		 name)
	  (list-of-fields fields))
       (for-each %field-is-a-symbol? fields)
       (if (symbol-bound? uid)
	   (let ((rtd (symbol-value uid)))
	     (unless (and (string=? name (struct-type-name rtd))
			  (equal? fields (struct-type-field-names rtd)))
	       (assertion-violation __who__
		 "mismatching data structure definition, \
                 selected UID already bound non type descriptor \
                 value or to type descriptor not matching this \
                 definition"))
	     rtd)
	 (receive-and-return (rtd)
	     ($make-rtd name fields uid)
	   (set-symbol-value! uid rtd))))))

  (define (%field-is-a-symbol? x)
    (unless (symbol? x)
      (assertion-violation 'make-struct-type "not a valid field name" x)))

  #| end of module: |# )

(define (struct-type-descriptor? obj)
  ;;Return true  if OBJ  is a struct  type descriptor;  otherwise return
  ;;false.
  ;;
  (and ($struct? obj)
       (eq? ($struct-rtd obj) (base-rtd))))

(define* (struct-type-name (std struct-type-descriptor?))
  ;;Return a string represnting the name of structures of type RTD.
  ;;
  ($std-name std))

(define* (struct-type-symbol (std struct-type-descriptor?))
  ;;Return a symbol uniquely identifying the data structure type STD.
  ;;
  ($std-symbol std))

(define* (struct-type-field-names (std struct-type-descriptor?))
  ;;Return  a  list of  symbols  representing  the  names of  fields  in
  ;;structures of type STD.
  ;;
  ($std-fields std))

(define* (struct-type-destructor (std struct-type-descriptor?))
  ;;Return false or a procedure being the destructor of STD.
  ;;
  ($std-destructor std))

;;; --------------------------------------------------------------------

(define* (set-rtd-printer! (std struct-type-descriptor?) (printer procedure?))
  ;;Select the procedure PRINTER as  printer for data structures of type
  ;;RTD.   The printer  accepts  as  3 arguments:  the  structure to  be
  ;;printed,  the port  to  which  write a  string  represention of  the
  ;;structure with DISPLAY,  a function to be optionally  applied to the
  ;;field values to print them.
  ;;
  ($set-std-printer! std printer))

(define* (set-rtd-destructor! (std struct-type-descriptor?) (destructor procedure?))
  ;;Select the procedure DESTRUCTOR ad destructor for data structures of
  ;;type  RTD.   The destructor  accepts  a  single argument  being  the
  ;;structure instance.
  ;;
  ($set-std-destructor! std destructor))


;;;; data structure functions

(module (struct-constructor)
  ;;Return a constructor function for  data structures of type STD.  The
  ;;constructor accepts as  many arguments as fields defined  by STD and
  ;;returns a new structure instance.
  ;;
  (define* (struct-constructor (std struct-type-descriptor?))
    (lambda args
      (let* ((field-num ($std-length std))
	     (stru      ($make-struct std field-num)))
	(if (%set-fields stru args 0 field-num)
	    ;;Notice  that the  expander also  has this  operation in  its
	    ;;implementation of DEFINE-STRUCT.
	    (if ($std-destructor std)
		($struct-guardian stru)
	      stru)
	  (assertion-violation __who__
	    "incorrect number of arguments to the constructor" std)))))

  (define (%set-fields stru field-val* field-idx field-num)
    (cond ((null? field-val*)
	   (if ($fx= field-idx field-num)
	       stru
	     #f))
	  (($fx< field-idx field-num)
	   (if (null? field-val*)
	       #f
	     (begin
	       ($struct-set! stru field-idx ($car field-val*))
	       (%set-fields stru ($cdr field-val*) ($fxadd1 field-idx) field-num))))
	  (else #f)))

  #| end of module: STRUCT-CONSTRUCTOR |# )

(define* (struct-predicate (std struct-type-descriptor?))
  ;;Return a predicate function for structures of type STD.
  ;;
  (lambda (obj)
    (and ($struct? obj)
	 (eq? std ($struct-rtd obj)))))

(module (struct-field-accessor
	 struct-field-mutator)

  (define* (struct-field-accessor (std struct-type-descriptor?) index/name)
    ;;Return an accessor  function for the field at  index INDEX/NAME of
    ;;data structures of type STD.
    ;;
    (let ((field-idx (%field-index index/name std __who__)))
      (lambda (x)
	(with-arguments-validation (__who__)
	    ((struct-of-type x std))
	  ($struct-ref x field-idx)))))

  (define* (struct-field-mutator (std struct-type-descriptor?) index/name)
    ;;Return a  mutator function  for the field  at index  INDEX/NAME of
    ;;data structures of type RTD.
    ;;
    (let ((field-idx (%field-index index/name std __who__)))
      (lambda (x v)
	(with-arguments-validation (__who__)
	    ((struct-of-type x std))
	  ($struct-set! x field-idx v)))))

  (define (%field-index index/name std who)
    (cond ((fixnum? index/name)
	   (unless (and ($fx>= index/name 0)
			($fx<  index/name ($std-length std)))
	     (procedure-argument-violation who
	       "struct field index out of range for std" index/name std))
	   index/name)
	  ((symbol? index/name)
	   (let loop ((field-idx   0)
		      (field-name* ($std-fields std)))
	     (cond ((null? field-name*)
		    (procedure-argument-violation who
		      "not a struct field name" index/name std))
		   ((eq? index/name ($car field-name*))
		    field-idx)
		   (else
		    (loop ($fxadd1 field-idx) ($cdr field-name*))))))
	  (else
	   (procedure-argument-violation who
	     "not a valid struct field index/name" index/name std))))

  #| end of module |# )

(define (struct-reset x)
  ;;Reset to #f all the fields of a structure.
  ;;
  (define who 'struct-reset)
  (with-arguments-validation (who)
      ((struct x))
    (let ((len ($struct-ref ($struct-rtd x) 1)))
      (do ((i 0 (+ 1 i)))
	  ((= i len))
	($struct-set! x i (void))))))


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
    (let ((rtd ($car rest)))
      (unless (null? ($cdr rest))
	(procedure-argument-violation who "too many arguments"))
      (unless (struct-type-descriptor? rtd)
	(procedure-argument-violation who "not an rtd"))
      (and ($struct? x)
	   (eq? rtd ($struct-rtd x))))))

(define (struct-rtd x)
  ;;Return the RTD  of the data structure X.   Notice that this function
  ;;works with both Vicare's structs and R6RS records.
  ;;
  (define who 'struct-rtd)
  (with-arguments-validation (who)
      ((struct x))
    ($struct-rtd x)))

(define (struct-length x)
  ;;Return the  number of fields in  the data structure  X.  Notice that
  ;;this function works with both Vicare's structs and R6RS records.
  ;;
  (define who 'struct-length)
  (with-arguments-validation (who)
      ((struct x))
    ($std-length ($struct-rtd x))))

(define (struct-name x)
  ;;Return  a string  representing the  name  of the  data structure  X.
  ;;Notice that this function works  with both Vicare's structs and R6RS
  ;;records.
  ;;
  (define who 'struct-name)
  (with-arguments-validation (who)
      ((struct x))
    ($std-name ($struct-rtd x))))

(define (struct-printer x)
  ;;Return  the  procedure  being  the  printer function  for  the  data
  ;;structure X.
  ;;
  (define who 'struct-printer)
  (with-arguments-validation (who)
      ((struct x))
    ($std-printer ($struct-rtd x))))

(define (struct-destructor x)
  ;;Return  the procedure  being the  destructor function  for the  data
  ;;structure X.
  ;;
  (define who 'struct-destructor)
  (with-arguments-validation (who)
      ((struct x))
    ($std-destructor ($struct-rtd x))))

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

(define (struct=? obj1 obj2)
  ;;Return true if OBJ1 and OBJ2  are two structures having the same RTD
  ;;and equal field values according to EQV?.
  ;;
  (and (struct? obj1)
       (struct? obj2)
       (eq? (struct-rtd obj1)
	    (struct-rtd obj2))
       (let ((len (struct-length obj1)))
	 (let loop ((i 0))
	   (or (= i len)
	       (and (eqv? (struct-ref obj1 i)
			  (struct-ref obj2 i))
		    (loop (+ 1 i))))))))

;;; --------------------------------------------------------------------

(define* (default-struct-printer (stru struct?) (port output-port?) unused)
  (with-arguments-validation (__who__)
      ((struct	stru))
    (let ((std ($struct-rtd stru)))
      (display (if (eq? std (base-rtd))
		   "#[std type="
		 "#[struct type=")
	       port)
      (display ($std-name std) port)
      (do ((i 0 ($fxadd1 i))
	   (field-names ($std-fields std) (cdr field-names)))
	  ((null? field-names))
	(display " " port)
	(display (car field-names) port)
	(display "=" port)
	(display ($struct-ref stru i) port))
      (display "]" port))))


;;;; done

;;Initialise the fields of the base RTD.
($set-std-name!       (base-rtd) "base-rtd")
($set-std-fields!     (base-rtd) '(name length fields printer symbol destructor))
($set-std-destructor! (base-rtd) #f)

($set-std-printer! (base-rtd)
		   default-struct-printer)
;; ($set-std-printer! (base-rtd)
;; 		   (lambda (rtd port wr)
;; 		     (define who 'struct-type-printer)
;; 		     (with-arguments-validation (who)
;; 			 ((rtd rtd))
;; 		       (display "#<" port)
;; 		       (display ($std-name rtd) port)
;; 		       (display " rtd>" port))))

#| end of libray (ikarus structs) |# )


(library (ikarus systems structs)
  (export $struct-ref $struct/rtd?)
  (import (ikarus))
  (define $struct-ref struct-ref)
  (define ($struct/rtd? x rtd)
    (import (ikarus system $structs))
    ($struct/rtd? x rtd)))

;;; end of file
