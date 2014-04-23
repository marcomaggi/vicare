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


#!vicare
(library (ikarus structs)
  (export
    ;; struct type descriptor constructor
    make-struct-type

    ;; struct type descriptor inspection
    struct-type-descriptor?
    struct-type-name		struct-type-symbol
    struct-type-field-names	struct-type-destructor

    ;; struct type descriptor customisation
    default-struct-printer	default-struct-printer-details
    set-rtd-printer!		set-rtd-destructor!

    ;; struct constructor and predicate
    struct?
    struct-constructor		struct-predicate
    struct=?

    ;; struct accessors and mutators
    struct-ref			struct-set!
    struct-field-accessor	struct-field-mutator
    struct-reset

    ;; structure inspection
    struct-rtd
    struct-name			struct-printer
    struct-destructor		struct-length)
  (import (except (vicare)
		  ;; struct type descriptor constructor
		  make-struct-type

		  ;; struct type descriptor inspection
		  struct-type-descriptor?
		  struct-type-name		struct-type-symbol
		  struct-type-field-names	struct-type-destructor

		  ;; struct type descriptor customisation
		  default-struct-printer	default-struct-printer-details
		  set-rtd-printer!		set-rtd-destructor!

		  ;; struct accessors and mutators
		  struct?			struct=?
		  struct-constructor		struct-predicate

		  ;; struct accessors and mutators
		  struct-ref			struct-set!
		  struct-field-accessor		struct-field-mutator
		  struct-reset

		  ;; structure inspection
		  struct-rtd			struct-type-descriptor
		  struct-name			struct-printer
		  struct-destructor		struct-length)
    (vicare language-extensions syntaxes)
    (vicare unsafe operations)
    (vicare system $structs)
    (only (vicare system $symbols)
	  $set-symbol-value!
	  $symbol-value))



;;;; helpers

(define (list-of-symbols? obj)
  (or (null? obj)
      (and (pair? obj)
	   (symbol? ($car obj))
	   (list-of-symbols? ($cdr obj)))))

(define-argument-validation (struct-of-type who struct std)
  ($struct/rtd? struct std)
  (procedure-argument-violation who "not a data structure of correct type" struct std))

(define-argument-validation (index who index struct)
  (and (fixnum? index)
       ($fx>= index 0)
       ($fx<  index ($std-length ($struct-rtd struct))))
  (procedure-argument-violation who
    "expected fixnum in range for structure field as index argument" index struct))


;;;; low level RTD operations

(define ($make-std name field-name* uid)
  (receive-and-return (std)
      ($struct (base-rtd) name (length field-name*) field-name*
	       default-struct-printer uid #f)
    ($set-symbol-value! uid std)))

;;; --------------------------------------------------------------------
;;; unsafe STD fields accessors

;; (define-syntax-rule ($std-name std)
;;   ($struct-ref std 0))

;; (define-syntax-rule ($std-length std)
;;   ($struct-ref std 1))

;; (define-syntax-rule ($std-fields std)
;;   ($struct-ref std 2))

;; (define-syntax-rule ($std-printer std)
;;   ($struct-ref std 3))

;; (define-syntax-rule ($std-symbol std)
;;   ($struct-ref std 4))

;; (define-syntax-rule ($std-destructor std)
;;   ($struct-ref std 5))

;;; --------------------------------------------------------------------
;;; unsafe STD fields mutators

;; (define-syntax-rule ($set-std-name! std name)
;;   ($struct-set! std 0 name))

;; (define-syntax-rule ($set-std-length! std n)
;;   ($struct-set! std 1 n))

;; (define-syntax-rule ($set-std-fields! std fields)
;;   ($struct-set! std 2 fields))

;; (define-syntax-rule ($set-std-printer! std printer)
;;   ($struct-set! std 3 printer))

;; (define-syntax-rule ($set-std-symbol! std symbol)
;;   ($struct-set! std 4 symbol))

;; (define-syntax-rule ($set-std-destructor! std destructor-func)
;;   ($struct-set! std 5 destructor-func))


;;;; structure type descriptor

(case-define* make-struct-type
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
  (({name string?} {field-name* list-of-symbols?})
   ($make-std name field-name* (gensym name)))

  (({name string?} {field-name* list-of-symbols?} {uid symbol?})
   (if (symbol-bound? uid)
       (let ((std ($symbol-value uid)))
	 (unless (and (string=? name (struct-type-name std))
		      (equal? field-name* (struct-type-field-names std)))
	   (assertion-violation __who__
	     "mismatching data structure definition, \
              selected UID already bound non type descriptor \
              value or to type descriptor not matching this \
              definition"))
	 std)
     ($make-std name field-name* uid))))

(define (struct-type-descriptor? obj)
  ;;Return true  if OBJ  is a struct  type descriptor;  otherwise return
  ;;false.
  ;;
  ($struct/rtd? obj (base-rtd)))

(define* (struct-type-name {std struct-type-descriptor?})
  ;;Return a string representing the name of structures of type RTD.
  ;;
  ($std-name std))

(define* (struct-type-symbol {std struct-type-descriptor?})
  ;;Return a symbol uniquely identifying the data structure type STD.
  ;;
  ($std-symbol std))

(define* (struct-type-field-names {std struct-type-descriptor?})
  ;;Return  a  list of  symbols  representing  the  names of  fields  in
  ;;structures of type STD.
  ;;
  ($std-fields std))

(define* (struct-type-destructor {std struct-type-descriptor?})
  ;;Return false or a procedure being the destructor of STD.
  ;;
  ($std-destructor std))

;;; --------------------------------------------------------------------

(define* (set-rtd-printer! {std struct-type-descriptor?} {printer procedure?})
  ;;Select the procedure PRINTER as  printer for data structures of type
  ;;RTD.   The printer  accepts  as  3 arguments:  the  structure to  be
  ;;printed,  the port  to  which  write a  string  represention of  the
  ;;structure with DISPLAY,  a function to be optionally  applied to the
  ;;field values to print them.
  ;;
  ($set-std-printer! std printer))

(define* (set-rtd-destructor! {std struct-type-descriptor?} {destructor procedure?})
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
  (define* (struct-constructor {std struct-type-descriptor?})
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

(define* (struct-predicate {std struct-type-descriptor?})
  ;;Return a predicate function for structures of type STD.
  ;;
  (lambda (obj)
    ($struct/rtd? obj std)))

(module (struct-field-accessor
	 struct-field-mutator)

  (define* (struct-field-accessor {std struct-type-descriptor?} index/name)
    ;;Return an accessor  function for the field at  index INDEX/NAME of
    ;;data structures of type STD.
    ;;
    (let ((field-idx (%field-index index/name std __who__)))
      (lambda (x)
	(with-arguments-validation (__who__)
	    ((struct-of-type x std))
	  ($struct-ref x field-idx)))))

  (define* (struct-field-mutator {std struct-type-descriptor?} index/name)
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

(define* (struct-reset {x struct?})
  ;;Reset to void all the fields of a structure.
  ;;
  (let ((len ($struct-ref ($struct-rtd x) 1)))
    (do ((i 0 ($fxadd1 i)))
	(($fx= i len))
      ($struct-set! x i (void)))))


;;;; data structure inspection

(case-define* struct?
  ;;When no STD argument is given return  true if X is a data structure.
  ;;When RSD is given: return true if X is a data structure and it is of
  ;;type STD.
  ;;
  ((x)
   ($struct? x))
  ((x {std struct-type-descriptor?})
   ($struct/rtd? x std)))

(define* (struct-rtd {stru struct?})
  ;;Return  the  STD of  the  data  structure  STRU.  Notice  that  this
  ;;function works with both Vicare's structs and R6RS records.
  ;;
  ($struct-rtd stru))

(define* (struct-length {stru struct?})
  ;;Return the number of fields in the data structure STRU.  Notice that
  ;;this function works with both Vicare's structs and R6RS records.
  ;;
  ($std-length ($struct-rtd stru)))

(define* (struct-name {stru struct?})
  ;;Return a  string representing the  name of the data  structure STRU.
  ;;Notice that this function works  with both Vicare's structs and R6RS
  ;;records.
  ;;
  ($std-name ($struct-rtd stru)))

(define* (struct-printer {stru struct?})
  ;;Return  the  procedure  being  the printer  function  for  the  data
  ;;structure STRU.
  ;;
  ($std-printer ($struct-rtd stru)))

(define* (struct-destructor {stru struct?})
  ;;Return  the procedure  being the  destructor function  for the  data
  ;;structure STRU.
  ;;
  ($std-destructor ($struct-rtd stru)))

(define* (struct-ref {stru struct?} i)
  ;;Return the value of field at index I in the data structure stru.
  ;;
  (with-arguments-validation (__who__)
      ((index	i stru))
    ($struct-ref stru i)))

(define* (struct-set! {stru struct?} i v)
  ;;Store V in the field at index I in the data structure X.
  ;;
  (with-arguments-validation (__who__)
      ((index	i stru))
    ($struct-set! stru i v)))

(define (struct=? obj1 obj2)
  ;;Return true if OBJ1 and OBJ2  are two structures having the same STD
  ;;and equal field values according to EQV?.
  ;;
  (and (struct? obj1)
       (struct? obj2)
       (let ((std1 ($struct-rtd obj1)))
	 (and (eq? std1 ($struct-rtd obj2))
	      (let ((len ($std-length std1)))
		(let loop ((i 0))
		  (or ($fx= i len)
		      (and (eqv? ($struct-ref obj1 i)
				 ($struct-ref obj2 i))
			   (loop ($fxadd1 i))))))))))

;;; --------------------------------------------------------------------

(define default-struct-printer-details
  (make-parameter #f
    (lambda (obj)
      (and obj #t))))

(define* (default-struct-printer {stru struct?} {port output-port?} unused)
  (let ((std ($struct-rtd stru)))
    (if (default-struct-printer-details)
	;;Long variant.
	(begin
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
	  (display "]" port))
      ;;Short variant.
      (if (eq? std (base-rtd))
	  ;;struct type descriptor
	  (begin
	    (display "#[std type=" port)
	    (display ($std-name std) port)
	    (display " " port)
	    (display "name=" port)
	    (display ($struct-ref stru 0) port)
	    (display "]" port))
	;;struct instance
	(begin
	  (display "#[struct type=" port)
	  (display ($std-name std) port)
	  (display "]" port))))))


;;;; done

;;Initialise the fields of the base RTD.
($set-std-name!       (base-rtd) "base-rtd")
($set-std-fields!     (base-rtd) '(name length fields printer symbol destructor))
;; (let ((uid (gensym "base-rtd")))
;;   ($set-std-symbol! (base-rtd) uid)
;;   ($set-symbol-value! uid (base-rtd)))
($set-std-destructor! (base-rtd) #f)
($set-std-printer!    (base-rtd) default-struct-printer)

#| end of libray (ikarus structs) |# )


(library (vicare systems structs)
  (export $struct-ref $struct/rtd?)
  (import (vicare))
  (define $struct-ref struct-ref)
  (define ($struct/rtd? x rtd)
    (import (vicare system $structs))
    ($struct/rtd? x rtd)))

;;; end of file
