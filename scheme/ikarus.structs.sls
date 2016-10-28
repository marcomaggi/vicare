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

    ;; struct type descriptor inspection and customisation
    struct-type-descriptor?
    struct-type-name			struct-type-symbol
    struct-type-field-names
    struct-type-printer			set-struct-type-printer!
    struct-type-destructor		set-struct-type-destructor!
    (rename
     (set-struct-type-printer!		set-rtd-printer!)
     (set-struct-type-destructor!	set-rtd-destructor!))

    ;; struct constructor and predicate
    struct?				struct-and-std?
    struct-constructor			struct-predicate
    struct=?				struct!=?

    ;; struct accessors and mutators
    struct-ref				struct-set!
    struct-and-std-ref			struct-and-std-set!
    struct-field-accessor		struct-field-mutator
    struct-field-method			struct-reset!

    ;; structure inspection
    struct-std
    (rename (struct-std	struct-rtd))
    struct-name				struct-length
    struct-field-names
    struct-printer			struct-destructor

    ;; unsafe operations
    $make-clean-struct
    $std-std
    $std-name			$set-std-name!
    $std-length			$set-std-length!
    $std-fields			$set-std-fields!
    $std-printer		$set-std-printer!
    $std-symbol			$set-std-symbol!
    $std-destructor		$set-std-destructor!

    ;; syntactic bindings for internal use only
    internal-applicable-struct-type-destructor)
  (import (except (vicare)
		  ;;This is redefined in this library.
		  list-of-symbols?

		  ;; struct type descriptor constructor
		  make-struct-type

		  ;; struct type descriptor inspection
		  struct-type-descriptor?
		  struct-type-name		struct-type-symbol
		  struct-type-field-names
		  struct-type-printer		set-struct-type-printer!
		  struct-type-destructor	set-struct-type-destructor!

		  ;; struct accessors and mutators
		  struct?			struct-and-std?
		  struct-constructor		struct-predicate
		  struct=?			struct!=?

		  ;; struct accessors and mutators
		  struct-ref			struct-set!
		  struct-and-std-ref			struct-and-std-set!
		  struct-field-accessor		struct-field-mutator
		  struct-field-method
		  struct-reset!

		  ;; structure inspection
		  struct-type-descriptor
		  struct-rtd			struct-std
		  struct-name			struct-length
		  struct-field-names
		  struct-printer		struct-destructor)
    (vicare system $fx)
    (vicare system $pairs)
    (except (vicare system $structs)
	    $make-clean-struct
	    $std-std
	    $std-name		$set-std-name!
	    $std-length		$set-std-length!
	    $std-fields		$set-std-fields!
	    $std-printer	$set-std-printer!
	    $std-symbol		$set-std-symbol!
	    $std-destructor	$set-std-destructor!)
    (only (vicare system $symbols)
	  $set-symbol-value!
	  $symbol-value)
    (prefix (only (ikarus.immutable-pairs)
		  <ipair> icar icdr ipair?)
	    ipair::)
    (only (vicare language-extensions syntaxes)
	  define-list-of-type-predicate
	  define-equality/sorting-predicate
	  define-inequality-predicate)
    #| end of IMPORT |# )


;;;; helpers

(define-list-of-type-predicate list-of-symbols? symbol?)

(define (false-or-procedure? obj)
  (or (not obj) (procedure? obj)))

(define-syntax (assert-struct-of-type stx)
  (syntax-case stx ()
    ((_ ?struct ?std)
     (and (identifier? #'?struct)
	  (identifier? #'?std))
     #'(unless ($struct/rtd? ?struct ?std)
	 (procedure-arguments-consistency-violation __who__
	   "not a data structure of correct type" ?struct ?std)))
    ))

(define-syntax (assert-field-index-for-struct stx)
  (syntax-case stx ()
    ((_ ?index ?struct)
     (and (identifier? #'?index)
	  (identifier? #'?struct))
     #'(unless (and (fixnum? ?index)
		    ($fx>= ?index 0)
		    ($fx<  ?index ($std-length ($struct-rtd ?struct))))
	 (procedure-arguments-consistency-violation __who__
	   "expected fixnum in range for structure field as INDEX argument"
	   ?index ?struct)))
    ))

(define-syntax (assert-struct-and-std stx)
  (syntax-case stx ()
    ((_ ?stru ?std)
     (and (identifier? #'?stru)
	  (identifier? #'?std))
     #'(unless ($struct/rtd? ?stru ?std)
	 (procedure-arguments-consistency-violation __who__
	   "struct is not an instance of given struct-type descriptor"
	   ?stru ?std)))
    ))


;;;; low level struct-type descriptor operations

(define ($make-std name field-name* uid)
  (receive-and-return (std)
      ($struct (base-rtd)
	       name		    ;index 0
	       (length field-name*) ;index 1, number of fields
	       field-name*	    ;index 2, list of field names as symbols
	       #f		    ;index 3, custom printer
	       uid		    ;index 4, unique symbol identifier
	       #f)		    ;index 5, destructor
    ($set-symbol-value! uid std)))

;;; --------------------------------------------------------------------
;;; unsafe STD fields accessors

;;These are both primitive functions and primitive operations.

(define ($std-std std)
  ($struct-rtd std))

(define ($std-name std)
  ($struct-ref std 0))

(define ($std-length std)
  ($struct-ref std 1))

(define ($std-fields std)
  ($struct-ref std 2))

(define ($std-printer std)
  ($struct-ref std 3))

(define ($std-symbol std)
  ($struct-ref std 4))

(define ($std-destructor std)
  ($struct-ref std 5))

;;; --------------------------------------------------------------------
;;; unsafe STD fields mutators

;;These are both primitive functions and primitive operations.

(define ($set-std-name! std name)
  ($struct-set! std 0 name))

(define ($set-std-length! std n)
  ($struct-set! std 1 n))

(define ($set-std-fields! std fields)
  ($struct-set! std 2 fields))

(define ($set-std-printer! std printer)
  ($struct-set! std 3 printer))

(define ($set-std-symbol! std symbol)
  ($struct-set! std 4 symbol))

(define ($set-std-destructor! std destructor-func)
  ($struct-set! std 5 destructor-func))


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
       (receive-and-return (std)
	   ($symbol-value uid)
	 (unless (and (string=? name (struct-type-name std))
		      (equal? field-name* (struct-type-field-names std)))
	   (assertion-violation __who__
	     "mismatching data structure definition, \
              selected UID already bound to non type-descriptor \
              value or to type-descriptor not matching this \
              definition")))
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

;;; --------------------------------------------------------------------

(define* (set-struct-type-printer! {std struct-type-descriptor?} {printer false-or-procedure?})
  ;;Select the procedure  PRINTER as printer for data structures  of type RTD; return
  ;;the old printer  function or false if  no printer function was set  for this STD.
  ;;The printer  accepts as  3 arguments: the  structure to be  printed, the  port to
  ;;which write a string represention of the structure with DISPLAY, a function to be
  ;;optionally applied to the field values to print them.
  ;;
  (begin0
      ($std-printer std)
    ($set-std-printer! std printer)))

(define* (struct-type-printer {std struct-type-descriptor?})
  ($std-printer std))

;;; --------------------------------------------------------------------

(define* (set-struct-type-destructor! {std struct-type-descriptor?} {destructor procedure?})
  ;;Select the  procedure DESTRUCTOR ad destructor  for data structures of  type RTD;
  ;;return the  old destructor function  or false if no  destructor was set  for this
  ;;STD.  The destructor accepts a single argument being the structure instance.
  ;;
  (begin0
      ($std-destructor std)
    ($set-std-destructor! std destructor)))

(define* (struct-type-destructor {std struct-type-descriptor?})
  ;;Return false or a procedure being the destructor of STD.
  ;;
  ($std-destructor std))

(define (internal-applicable-struct-type-destructor std)
  ;;This private primitive is  used by the DELETE syntax.  If a  destructor is set in
  ;;STD: return it; otherwise  return a function that does nothing.   This way we can
  ;;use  DELETE  on  both  structs  having  a destructor  and  struct  not  having  a
  ;;destructor.
  ;;
  (or ($std-destructor std)
      (lambda (x) (void))))


;;;; data structure functions

(define ($make-clean-struct std)
  (foreign-call "ikrt_make_struct" std))

(module (struct-constructor)
  ;;Return a constructor function for  data structures of type STD.  The
  ;;constructor accepts as  many arguments as fields defined  by STD and
  ;;returns a new structure instance.
  ;;
  (define* (struct-constructor {std struct-type-descriptor?})
    (lambda args
      (let ((stru ($make-clean-struct std)))
	(if (%set-fields stru args 0 ($std-length std))
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
	 struct-field-mutator
	 struct-field-method)

  (define* (struct-field-accessor {std struct-type-descriptor?} index/name)
    ;;Return an accessor  function for the field at  index INDEX/NAME of
    ;;data structures of type STD.
    ;;
    (let ((field-idx (%field-index index/name std __who__)))
      (lambda (x)
	(fluid-let-syntax
	    ((__who__ (identifier-syntax 'anonymous-struct-accessor)))
	  (assert-struct-of-type x std)
	  ($struct-ref x field-idx)))))

  (define* (struct-field-mutator {std struct-type-descriptor?} index/name)
    ;;Return a  mutator function  for the field  at index  INDEX/NAME of
    ;;data structures of type STD.
    ;;
    (let ((field-idx (%field-index index/name std __who__)))
      (lambda (x v)
	(fluid-let-syntax
	    ((__who__ (identifier-syntax 'anonymous-struct-mutator)))
	  (assert-struct-of-type x std)
	  ($struct-set! x field-idx v)))))

  (define* (struct-field-method {std struct-type-descriptor?} index/name)
    ;;Return a method  function for the field at index  INDEX/NAME of data structures
    ;;of type STD.
    ;;
    (let ((field-idx (%field-index index/name std __who__)))
      (fluid-let-syntax
	  ((__who__ (identifier-syntax 'anonymous-struct-field-method)))
	(case-lambda
	 ((x)
	  (assert-struct-of-type x std)
	  ($struct-ref x field-idx))
	 ((x v)
	  (assert-struct-of-type x std)
	  ($struct-set! x field-idx v))))))

  (define (%field-index index/name std who)
    (cond ((fixnum? index/name)
	   (unless (and ($fx>= index/name 0)
			($fx<  index/name ($std-length std)))
	     (procedure-arguments-consistency-violation who
	       "struct field index out of range for std" index/name std))
	   index/name)
	  ((symbol? index/name)
	   (let loop ((field-idx   0)
		      (field-name* ($std-fields std)))
	     (cond ((null? field-name*)
		    (procedure-arguments-consistency-violation who
		      "not a struct field name" index/name std))
		   ((eq? index/name ($car field-name*))
		    field-idx)
		   (else
		    (loop ($fxadd1 field-idx) ($cdr field-name*))))))
	  (else
	   (procedure-arguments-consistency-violation who
	     "not a valid struct field index/name" index/name std))))

  #| end of module |# )

(define* (struct-reset! {x struct?})
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

(define* (struct-and-std? stru {std struct-type-descriptor?})
  ($struct/rtd? stru std))

(define* (struct-std {stru struct?})
  ;;Return  the  STD of  the  data  structure  STRU.  Notice  that  this
  ;;function works with both Vicare's structs and R6RS records.
  ;;
  ($struct-rtd stru))

(define* (struct-length {stru struct?})
  ;;Return the number of fields in the data structure STRU.  Notice that
  ;;this function works with both Vicare's structs and R6RS records.
  ;;
  ($std-length ($struct-rtd stru)))

(define* (struct-field-names {stru struct?})
  ;;Return a list of symbols representing the structure field names.
  ;;
  ($std-fields ($struct-rtd stru)))

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
  ;;Return  false  or the  procedure  being  the  destructor  function for  the  data
  ;;structure STRU.
  ;;
  ($std-destructor ($struct-rtd stru)))

(define* (struct-ref {stru struct?} i)
  ;;Return the value of field at index I in the data structure stru.
  ;;
  (assert-field-index-for-struct i stru)
  ($struct-ref stru i))

(define* (struct-and-std-ref {stru struct?} i {std struct-type-descriptor?})
  (assert-struct-and-std stru std)
  (assert-field-index-for-struct i stru)
  ($struct-ref stru i))

(define* (struct-set! {stru struct?} i v)
  ;;Store V in the field at index I in the data structure X.
  ;;
  (assert-field-index-for-struct i stru)
  ($struct-set! stru i v))

(define* (struct-and-std-set! {stru struct?} i {std struct-type-descriptor?} new-value)
  (assert-struct-and-std stru std)
  (assert-field-index-for-struct i stru)
  ($struct-set! stru i new-value))


;;;; comparison

(define-equality/sorting-predicate struct=?	$struct=	struct?)
(define-inequality-predicate       struct!=?	$struct!=	struct?)

(define ($struct= stru1 stru2)
  ;;Return true if STRU1  and STRU2 are two structures having the  same STD and equal
  ;;field values according to EQV?.
  ;;
  (let ((std1 ($struct-rtd stru1)))
    (and (eq? std1 ($struct-rtd stru2))
	 (let ((len ($std-length std1)))
	   (let loop ((i 0))
	     (or ($fx= i len)
		 (and (eqv? ($struct-ref stru1 i)
			    ($struct-ref stru2 i))
		      (loop ($fxadd1 i)))))))))

(define ($struct!= stru1 stru2)
  (not ($struct= stru1 stru2)))


;;;; done

;; #!vicare
;; (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.structs before"))

;;Initialise the fields of the base RTD.
($set-std-name!		(base-rtd) "base-rtd")
($set-std-fields!	(base-rtd) '(name length fields printer symbol destructor))
($set-std-symbol!	(base-rtd) #f)
;; (let ((uid (expand-time-gensym "base-rtd")))
;;   ($set-std-symbol! (base-rtd) uid)
;;   ($set-symbol-value! uid (base-rtd)))
($set-std-destructor!	(base-rtd) #f)
($set-std-printer!	(base-rtd) #f)

;;; --------------------------------------------------------------------

($set-std-printer! (type-descriptor ipair::<ipair>)
		   (lambda (stru port sub-printer)
		     (define-syntax-rule (%display ?obj)
		       (display ?obj port))
		     (let ((A (ipair::icar stru))
			   (D (ipair::icdr stru)))
		       (cond ((ipair::ipair? D)
			      (%display "(imlist ")
			      (%display A)
			      (let loop ((D D))
				(%display #\space)
				(%display (ipair::icar D))
				(let ((DD (ipair::icdr D)))
				  (cond ((ipair::ipair? DD)
					 (loop DD))
					((null? DD)
					 (%display ")"))
					(else
					 (%display #\space)
					 (%display ". ")
					 (%display DD)
					 (%display ")"))))))
			     ((null? D)
			      (%display "(imlist ")
			      (%display A)
			      (%display ")"))
			     (else
			      (%display "(impair ")
			      (%display A)
			      (%display #\space)
			      (%display D)
			      (%display ")"))))))

;; #!vicare
;; (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.structs after"))

#| end of libray (ikarus structs) |# )


(library (vicare structs unsafe)
  (export $struct-ref $struct/rtd?)
  (import (vicare)
    (vicare system structs))
  (define $struct-ref struct-ref)
  (define ($struct/rtd? x rtd)
    (import (vicare system $structs))
    ($struct/rtd? x rtd)))

;;; end of file
