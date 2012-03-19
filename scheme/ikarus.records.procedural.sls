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


(library (ikarus records procedural)
  (export
    ;; bindings for (rnrs records procedural (6))
    make-record-type-descriptor		make-record-constructor-descriptor
    (rename (<rtd>? record-type-descriptor?))
    record-constructor			record-predicate
    record-accessor			record-mutator

    ;; bindings for (rnrs records inspection (6))
    record?				record-rtd
    record-type-name			record-type-parent
    record-type-uid			record-type-generative?
    record-type-sealed?			record-type-opaque?
    record-field-mutable?
    record-type-field-names

    ;; extension utility functions, non-R6RS
    rtd-subtype?)
  (import (except (ikarus)
		  ;; bindings for (rnrs records procedural (6))
		  make-record-type-descriptor		make-record-constructor-descriptor
		  record-type-descriptor?
		  record-constructor			record-predicate
		  record-accessor			record-mutator

		  ;; bindings for (rnrs records inspection (6))
		  record?				record-rtd
		  record-type-name			record-type-parent
		  record-type-uid			record-type-generative?
		  record-type-sealed?			record-type-opaque?
		  record-field-mutable?
		  record-type-field-names

		  ;; extension utility functions, non-R6RS
		  rtd-subtype?)
    (ikarus system $structs)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe-operations)
	    unsafe.))


;;;; type definitions
;;
;;A Vicare's struct  is a variable-length block of  memory referenced by
;;machine  words  tagged as  vectors;  the  first  machine word  of  the
;;structure  is a  reference to  the  struct-type  descriptor, which  is
;;itself a  struct.  A block  of memory  is a struct  if and only  if: a
;;reference to  it is tagged as vector  and its first word  is tagged as
;;vector.
;;
;;   |----------------|----------| reference to struct
;;     heap pointer    vector tag
;;
;;   |----------------|----------| first word of struct
;;     heap pointer    vector tag    = reference to struct-type desc
;;                                   = reference to struct
;;
;;The  struct-type  descriptor of  the  struct-type  descriptors is  the
;;return value  of BASE-RTD.  The graph  of references for  a struct and
;;its type descriptor is as follows:
;;
;;       std ref
;;      |-------|---------------| struct instance
;;          |
;;   ---<---
;;  |
;;  |     std ref
;;   -->|-------|---------------| struct-type descriptor
;;          |
;;   ---<---
;;  |
;;  |     std ref
;;  +-->|-------|---------------| base struct-type descriptor
;;  |       |
;;   ---<---
;;
;;An R6RS record-type descriptor is a struct instance of type <RTD>.  An
;;R6RS record instance is a  struct instance whose first word references
;;its type descriptor, that is a struct instance of type <RTD>.
;;
;;       RTD ref
;;      |-------|---------------| R6RS record instance
;;          |
;;   ---<---
;;  |
;;  |     std ref
;;   -->|-------|---------------| R6RS record-type descriptor
;;          |                      = struct instance of type <RTD>
;;   ---<---
;;  |
;;  |     std ref
;;  +-->|-------|---------------| <RTD> type descriptor
;;          |
;;   ---<---
;;  |
;;  |     std ref
;;  +-->|-------|---------------| base struct-type descriptor
;;  |       |
;;   ---<---
;;

(define-struct <rtd>
  ;;R6RS record type descriptor.
  (name
		;Record type name, for debugging purposes.
   size
		;Total  number of  fields, including  the fields  of the
		;parents.
   parent
		;False or  instance of RTD  structure, it is  the parent
		;RTD.
   sealed?
		;Boolean, true if this record type is sealed.
   opaque?
		;Boolean, true if this record type is opaque.
   uid
		;False or Scheme  symbol, unique record type identifier.
		;If it is a symbol: the RTD is nongenerative.
   fields
		;Scheme vector, normalised fields specification.
		;
		;It  is a  vector with  an element  for each  field; the
		;elements have the order  in which fields where given to
		;MAKE-RECORD-TYPE-DESCRIPTOR.   Each element  is  a pair
		;whose car is  a boolean, true if the  field is mutable,
		;and whose cdr is a Scheme symbol representing the field
		;name.
   ))

(define (%rtd-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[rtd")
  (%display " name=")		(%display (<rtd>-name S))
  (%display " size=")		(%display (<rtd>-size S))
  (let ((prtd (<rtd>-parent S)))
    (if (<rtd>? prtd)
	(begin
	  (%display " parent-name=")
	  (%display (<rtd>-name prtd)))
      (begin
	(%display " parent=")
	(%display prtd))))
  (%display " sealed?=")	(%display (<rtd>-sealed? S))
  (%display " opaque?=")	(%display (<rtd>-opaque? S))
  (%display " fields=")		(%display (<rtd>-fields  S))
  (%display "]"))

(define-struct <rcd>
  ;;R6RS record constructor descriptor.
  (rtd
		;A  struct  of  type  RTD,  the  associated  record-type
		;descriptor.
   parent-rcd
		;False   or   a  struct   of   type   RCD,  the   parent
		;record-constructor descriptor.
   protocol
		;A function, the protocol function for this RCD.
   ))

(define (%rcd-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[rcd")
  (%display " rtd=")		(%display (<rcd>-rtd S))
  (%display " prcd=")		(%display (<rcd>-parent-rcd S))
  (%display " protocol=")	(%display (<rcd>-protocol S))
  (%display "]"))


;;;; helpers

(define-constant $error-message-expected-rtd
  "expected record-type descriptor as argument")

(define-constant $error-message-expected-rcd
  "expected record-constructor descriptor as argument")

(define-constant $error-message-expected-record
  "expected record as argument")

(define (%fields-specification-vector? V)
  ;;Return  true if  V is  valid as  vector specifying  the fields  of a
  ;;record type.  A fields vector looks like this:
  ;;
  ;;   #((mutable   <symbol-1>)
  ;;     (immutable <symbol-2>)
  ;;     (mutable   <symbol-3>))
  ;;
  ;;Remember that a Vicare vector has at most (greatest-fixnum) items.
  ;;
  (and (vector? V)
       (let ((V.len (unsafe.vector-length V)))
	 (let next-item ((i 0))
	   (or (unsafe.fx= i V.len)
	       (let ((item (unsafe.vector-ref V i)))
		 (and (pair? item)
		      (let ((A (unsafe.car item)))
			(or (eq? A 'mutable)
			    (eq? A 'immutable)))
		      (let ((D (unsafe.cdr item)))
			(and (pair? D)
			     (null? (unsafe.cdr D))
			     (symbol? (unsafe.car D))
			     (next-item (unsafe.fxadd1 i)))))))))))

(define (%normalise-fields-vector input-vector)
  ;;Parse  the  fields specification  vector  and  return an  equivalent
  ;;vector in normalised format; this function assumes that INPUT-VECTOR
  ;;has already been validated as fields specification vector.  Example,
  ;;the input vector:
  ;;
  ;;	#((mutable a) (immutable b))
  ;;
  ;;is transformed into:
  ;;
  ;;  #((#t . a) (#f . b))
  ;;
  ;;the symbols  MUTABLE and  IMMUTABLE are respectively  converted into
  ;;true and false booleans, lists are converted to pairs.
  ;;
  (let* ((number-of-fields	(unsafe.vector-length input-vector))
	 (normalised-vector	(unsafe.make-vector   number-of-fields)))
    (let next-field ((i 0))
      (if (unsafe.fx= i number-of-fields)
	  normalised-vector
	(let* ((item		(unsafe.vector-ref input-vector i))
	       (mutability	(unsafe.car item))
	       (name		(unsafe.car (unsafe.cdr item))))
	  (unsafe.vector-set! normalised-vector i (cons (eq? mutability 'mutable) name))
	  (next-field (unsafe.fxadd1 i)))))))


;;;; arguments validation

(define-argument-validation (rtd who obj)
  (<rtd>? obj)
  (assertion-violation who $error-message-expected-rtd obj))

(define-argument-validation (false/non-sealed-parent-rtd who obj)
  (or (not obj)
      (and (<rtd>? obj)
	   (not (<rtd>-sealed? obj))))
  (assertion-violation who "expected false or non-sealed RTD as parent RTD argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (rcd who obj)
  (<rcd>? obj)
  (assertion-violation who "expected record-constructor descriptor as argument" obj))

(define-argument-validation (false/rcd who obj)
  (or (not obj) (<rcd>? obj))
  (assertion-violation who "expected false or record-constructor descriptor as argument" obj))

(define-argument-validation (rtd&prcd who rtd prcd)
  (eq? (<rtd>-parent rtd) (<rcd>-rtd prcd))
  (assertion-violation who
    "record-constructor descriptor is not associated to the parent of the record-type descriptor"
    rtd prcd))

;;; --------------------------------------------------------------------

(define-argument-validation (record who obj)
  (%record-object? obj)
  (assertion-violation who $error-message-expected-record obj))

(define-argument-validation (non-opaque-record who obj)
  (record? obj)
  (assertion-violation who "expected non-opaque record as argument" obj))

(define-argument-validation (built-record-and-rtd who record rtd)
  (eq? rtd ($struct-rtd record))
  (assertion-violation who
    "constructor function returned record of invalid type, expected record of specific RTD"
    record rtd))

(define-argument-validation (record-instance-of-rtd who record rtd)
  ;;To be called with RECORD being an R6RS record instance and RTD being
  ;;an R6RS record-type descriptor.
  ;;
  (let ((rtd^ ($struct-rtd record)))
    (or (eq? rtd rtd^)
	(let upper-parent ((prtd^ (<rtd>-parent rtd^)))
	  (and prtd^
	       (or (eq? prtd^ rtd) ;RECORD is an instance of a subtype of RTD
		   (upper-parent (<rtd>-parent prtd^)))))))
  (assertion-violation who
    "invalid record type as accessor or mutator argument" record rtd))

;;; --------------------------------------------------------------------

(define-argument-validation (record-type-name who obj)
  (symbol? obj)
  (assertion-violation who "expected symbol as record-type name argument" obj))

(define-argument-validation (sealed who obj)
  (boolean? obj)
  (assertion-violation who "expected boolean as sealed argument" obj))

(define-argument-validation (opaque who obj)
  (boolean? obj)
  (assertion-violation who "expected boolean as opaque argument" obj))

(define-argument-validation (uid who obj)
  (or (not obj) (symbol? obj))
  (assertion-violation who "expected false or symbol as UID argument" obj))

(define-argument-validation (protocol who obj)
  (or (not obj) (procedure? obj))
  (assertion-violation who "expected protocol function as argument" obj))

(define-argument-validation (fields-specification-vector who obj)
  (%fields-specification-vector? obj)
  (assertion-violation who "expected fields vector as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (index who obj)
  (and (fixnum? obj) (unsafe.fx<= 0 obj))
  (assertion-violation who "expected non-negative fixnum as field index argument" obj))

(define-argument-validation (absolute-field-index who abs-index max-index rtd relative-field-index)
  (and (fixnum? abs-index)
       (unsafe.fx< abs-index max-index))
  (assertion-violation who
    (string-append "absolute field index " (number->string abs-index)
		   " out of range, expected less than " (number->string max-index))
    rtd relative-field-index))

(define-argument-validation (relative-index-of-mutable-field who relative-field-index rtd)
  ;;To be called with RELATIVE-FIELD-INDEX being a valid field index for
  ;;RTD and RTD being an R6RS record-type descriptor.
  ;;
  ;;We have to remember that the RTD structure holds a normalised vector
  ;;of field specifications.
  ;;
  (unsafe.car (unsafe.vector-ref (<rtd>-fields rtd) relative-field-index))
  (assertion-violation who
    "selected record field is not mutable" relative-field-index rtd))


;;;; table of defined non-generative RTDs

(define-constant RTD-TABLE
  (make-eq-hashtable))

(define-inline (%intern-nongenerative-rtd! ?uid ?rtd)
  (let ((rtd ?rtd))
    (hashtable-set! RTD-TABLE ?uid rtd)
    rtd))

(define-inline (%lookup-nongenerative-rtd ?uid)
  (hashtable-ref RTD-TABLE ?uid #f))


;;;; record instance inspection

(define-inline (%record-object? ?obj)
  ;;Return #t if  OBJ is a record, else return #f.   Does not care about
  ;;the opaqueness of the record type.
  ;;
  (let ((obj ?obj))
    (and ($struct? obj)
	 (<rtd>? ($struct-rtd obj)))))

(define (record? x)
  ;;Defined by R6RS.   Return #t if OBJ is a record  and its record type
  ;;is not opaque, else return #f.
  ;;
  (and ($struct? x)
       (let ((rtd ($struct-rtd x)))
	 (and (<rtd>? rtd)
	      (not (<rtd>-opaque? rtd))))))

(define (record-rtd x)
  (define who 'record-rtd)
  (with-arguments-validation (who)
      ((non-opaque-record x))
    ($struct-rtd x)))


;;;; record-type descriptor inspection

(define record-type-descriptor? <rtd>?)

(define-syntax define-rtd-inspector
  (syntax-rules ()
    ((_ ?procname ?accessor)
     (define (?procname rtd)
       (define who '?procname)
       (with-arguments-validation (who)
	   ((rtd	rtd))
	 (?accessor rtd))))))

(define-inline (not-<rtd>-uid rtd)
  (not (<rtd>-uid rtd)))

(define-rtd-inspector record-type-name		<rtd>-name)
(define-rtd-inspector record-type-parent	<rtd>-parent)
(define-rtd-inspector record-type-sealed?	<rtd>-sealed?)
(define-rtd-inspector record-type-opaque?	<rtd>-opaque?)
(define-rtd-inspector record-type-generative?	not-<rtd>-uid)

;;Return  false or the  UID of  RTD.  Notice  that this  library assumes
;;that:  there is  a  difference between  generative and  non-generative
;;record-type  descriptors.  This  is what  R6RS  as to  say about  this
;;function:
;;
;;	Return the  uid of the record-type  descriptor RTD, or  #f if it
;;	has none.   (An implementation may  assign a generated UID  to a
;;	record type even  if the type is generative, so  the return of a
;;	UID does not necessarily imply that the type is nongenerative.)
;;
(define-rtd-inspector record-type-uid	<rtd>-uid)

(define (record-type-field-names rtd)
  ;;Return a vector holding one Scheme symbol for each field of RTD, not
  ;;including fields  of the  parents; the order  of the symbols  is the
  ;;same of the order of the fields in the RTD definition.
  ;;
  (define who 'record-type-field-names)
  (with-arguments-validation (who)
      ((rtd	rtd))
    (let* ((fields-vector	(<rtd>-fields rtd))
	   (number-of-fields	(unsafe.vector-length fields-vector)))
      (let next-field ((v (unsafe.make-vector number-of-fields))
		       (i 0))
	(if (unsafe.fx= i number-of-fields)
	    v
	  (begin
	    (unsafe.vector-set! v i (unsafe.cdr (unsafe.vector-ref fields-vector i)))
	    (next-field v (unsafe.fxadd1 i))))))))

(define (record-field-mutable? rtd field-index)
  ;;Return true if field FIELD-INDEX of RTD is mutable.
  ;;
  (define who 'record-field-mutable?)
  (with-arguments-validation (who)
      ((rtd	rtd)
       (index	field-index))
    (let* ((total-number-of-fields	(<rtd>-size   rtd))
	   (prtd			(<rtd>-parent rtd))
	   (absolute-field-index	(if prtd
					    (+ field-index (<rtd>-size prtd))
					  field-index)))
      (cond ((not (fixnum? absolute-field-index))
	     (assertion-violation who
	       "field index out of range" field-index))
	    ((fx<? absolute-field-index total-number-of-fields)
	     ;;Remember that the RTD structure holds a normalised vector
	     ;;of field specifications.
	     (unsafe.car (unsafe.vector-ref (<rtd>-fields rtd) field-index)))
	    (else
	     (assertion-violation who
	       "relative field index out of range for record type" field-index))))))


(define (make-record-type-descriptor name parent uid sealed? opaque? fields)
  ;;Return a record-type descriptor  representing a record type distinct
  ;;from all built-in types and other record types.
  ;;
  ;;See the R6RS document for details on the arguments.
  ;;
  (define who 'make-record-type-descriptor)

  (define-inline (main)
    (with-arguments-validation (who)
	((record-type-name		name)
	 (false/non-sealed-parent-rtd	parent)
	 (sealed			sealed?)
	 (opaque			opaque?)
	 (uid				uid)
	 (fields-specification-vector	fields))
      (let ((normalised-fields (%normalise-fields-vector fields)))
	(if (symbol? uid)
	    (%make-nongenerative-rtd name parent uid sealed? opaque? normalised-fields)
	  (%generate-rtd name parent uid sealed? opaque? normalised-fields)))))

  (define (%generate-rtd name parent uid sealed? opaque? normalised-fields)
    ;;Build and return a new instance of RTD struct.
    ;;
    ;;Here we do not care if UID is a symbol or false.
    ;;
    (define-inline (%make-rtd ?opaque? ?parent-size)
      (make-<rtd> name (fx+ ?parent-size (vector-length normalised-fields))
		parent sealed? ?opaque? uid normalised-fields))
    (if (not parent)
	(%make-rtd opaque? 0)
      (%make-rtd (or opaque? (<rtd>-opaque? parent)) (<rtd>-size parent))))

  (define (%make-nongenerative-rtd name parent uid sealed? opaque? normalised-fields)
    ;;Build  and return  a  new instance  of  RTD or  return an  already
    ;;generated RTD.   If the  specified UID is  already present  in the
    ;;table of  interned RTDs: check  that the arguments  are compatible
    ;;and return the interned RTD.
    ;;
    (let ((rtd (%lookup-nongenerative-rtd uid)))
      (define (%error wrong-field)
	(assertion-violation who
	  (string-append
	   "requested access to non-generative record-type descriptor \
            with " wrong-field " not equivalent to that in the interned RTD")
	  rtd `(,name ,parent ,uid ,sealed? ,opaque? ,fields)))
      (if rtd
	  ;;Should this  validation be  omitted when we  compile without
	  ;;arguments validation?  (Marco Maggi; Sun Mar 18, 2012)
	  ;;
	  ;;Notice that the requested NAME can be different from the one
	  ;;in the interned RTD.
	  (if (eq? (<rtd>-parent  rtd) parent)
	      (if (boolean=? (<rtd>-sealed? rtd) sealed?)
		  (if (boolean=? (<rtd>-opaque? rtd) opaque?)
		      (if (equal? (<rtd>-fields  rtd) normalised-fields)
			  rtd
			(%error "fields"))
		    (%error "opaque"))
		(%error "sealed"))
	    (%error "parent"))
	(%intern-nongenerative-rtd!
	 uid (%generate-rtd name parent uid sealed? opaque? normalised-fields)))))

  (main))


(define (make-record-constructor-descriptor rtd prcd protocol)
  ;;Return   a  record-constructor   descriptor   specifying  a   record
  ;;constructor, which  can be  used to construct  record values  of the
  ;;type   specified   by  RTD,   and   which   can   be  obtained   via
  ;;RECORD-CONSTRUCTOR.
  ;;
  ;;PRCD must be false or a record-constructor descriptor for the parent
  ;;of RTD; PROTOCOL must be a function.
  ;;
  (define who 'make-record-constructor-descriptor)
  (with-arguments-validation (who)
      ((rtd		rtd)
       (protocol	protocol)
       (false/rcd	prcd))
    (if (not prcd)
	(make-<rcd> rtd #f protocol)
      (with-arguments-validation (who)
	  ((rtd&prcd rtd prcd))
	(make-<rcd> rtd prcd protocol)))))


;;Let's say we have the following definition:
;;
;; (define rtd (make-record-type-descriptor 'name #f #f #f #f '#()))
;; (define num-of-fields (vector-length (record-type-field-names rtd)))
;;
;; (define protocol-wrapper
;;   (lambda (list-of-field-values-lists)
;;     (protocol (%make-allocator-function list-of-field-values-lists ?list-of-formals))))
;; (lambda )
;;   ($make-struct main-rtd (<rtd>-size main-rtd)))
;;
;;

(define (record-constructor main-rcd)
  (define who 'record-constructor)
  (define main-rtd)

  (define-inline (main)
    (with-arguments-validation (who)
	((rcd	main-rcd))
      (set! main-rtd (<rcd>-rtd main-rcd))
      ;;The protocol wrapper is applied  to null and the return value is
      ;;the constructor function.
      ((%protocol-wrapper (<rtd>-size main-rtd) (<rcd>-parent-rcd main-rcd) (<rcd>-protocol main-rcd))
       '())))

  (define (%protocol-wrapper number-of-fields prcd protocol)
    (if prcd
  	(%make-wrapper-for-protocol/with-parent number-of-fields prcd protocol)
      (%make-wrapper-for-protocol/without-parent number-of-fields protocol)))

  (define (%make-wrapper-for-protocol/without-parent number-of-fields protocol)
    ;;Return a constructor wrapper.
    ;;
    (define (sub-main)
      ;;When  a record-type  descriptor is  defined with  the procedural
      ;;layer, the number of fields is known only at runtime; this makes
      ;;it   impossible  to  generate   allocator  functions   with  one
      ;;initialisation form for each field.
      ;;
      ;;We distinguish the  case of "few fields" from  the case of "many
      ;;fields", because  of the  following hypothesis: when  few fields
      ;;exist, evaluating one initialisation form for each field is more
      ;;efficient than putting  the values in a list,  and act upon them
      ;;with loops.
      ;;
      (case number-of-fields
	((0)  (expand-one-case ()))
	((1)  (expand-one-case (arg0)))
	((2)  (expand-one-case (arg0 arg1)))
	((3)  (expand-one-case (arg0 arg1 arg2)))
	((4)  (expand-one-case (arg0 arg1 arg2 arg3)))
	(else (expand-one-case #f)))) ;more than 4

    (define-syntax expand-one-case
      (syntax-rules ()
	((_ ?list-of-formals)
	 (if protocol
	     (lambda (list-of-field-values-lists) ;protocol wrapper
	       (protocol (%make-allocator-function list-of-field-values-lists ?list-of-formals)))
	   (lambda (list-of-field-values-lists) ;protocol wrapper
	     (%make-allocator-function list-of-field-values-lists ?list-of-formals))))))

    ;; (let ((record (constructor list-of-field-values-lists ?list-of-formals)))
    ;;   (with-arguments-validation (who)
    ;; 	  ((record			record)
    ;; 	   (built-record-and-rtd	record main-rtd))
    ;; 	record)))))))

    (define-syntax %make-allocator-function
      ;;Expand into a LAMBDA form being the record allocator; the LAMBDA
      ;;accepts  a number  of arguments  equal  to the  total number  of
      ;;fields in the record.
      ;;
      (lambda (stx)
	(define (%iota count)
	  ;;Return a  list of  exact integers from  0 included  to COUNT
	  ;;excluded.
	  (let loop ((count	(- count 1))
		     (val	(- count 1))
		     (ret	'()))
	    (if (negative? count)
		ret
	      (loop (- count 1) (- val 1) (cons val ret)))))
	(define (%unwrap stx)
	  ;;Given  a  syntax object  STX  decompose  it  and return  the
	  ;;corresponding  S-expression holding datums  and identifiers.
	  ;;Take care  of returning  a proper list  when the input  is a
	  ;;syntax object holding a proper list.
	  ;;
	  (syntax-case stx ()
	    (() '())
	    ((?car . ?cdr)
	     (cons (%unwrap #'?car) (%unwrap #'?cdr)))
	    (#(?item ...)
	     (list->vector (%unwrap #'(?item ...))))
	    (?atom
	     (identifier? #'?atom)
	     #'?atom)
	    (?atom
	     (syntax->datum #'?atom))))
	(syntax-case stx ()
	  ;;The number of arguments/fields is between 0 and 4.
	  ((_ ?list-of-field-values-lists (?arg ...))
	   (identifier? #'?list-of-field-values-lists)
	   (let ((args.len (length (%unwrap #'(?arg ...)))))
	     (with-syntax (((INDEX ...)	(%iota args.len))
			   (NEXT-INDEX	args.len))
	       #'(lambda (?arg ...) ;allocator
		   (let ((the-record ($make-struct main-rtd (<rtd>-size main-rtd))))
		     ;;store the values of the base RTD fields
		     ($struct-set! the-record INDEX ?arg) ...
		     ;;if any, store the values of the sub-RTDs fields
		     (if (null? ?list-of-field-values-lists)
			 the-record
		       (%fill the-record NEXT-INDEX
			      (car ?list-of-field-values-lists)
			      (cdr ?list-of-field-values-lists))))))))

	  ;;The number of arguments/fields is more than 4.
	  ((_ ?list-of-field-values-lists #f)
	   (identifier? #'?list-of-field-values-lists)
	   #'(lambda formals	;allocator
	       (arguments-validation-forms
		(unless (fx=? (length formals) number-of-fields)
		  (apply assertion-violation 'a-record-constructor
			 (string-append "record allocator for \""
					(symbol->string (<rtd>-name main-rtd))
					"\" expected "
					(number->string number-of-fields)
					" arguments, but got "
					(number->string (length formals)))
			 formals)))
	       (let ((the-record ($make-struct main-rtd (<rtd>-size main-rtd))))
		 ;;Store the values of the  base RTD fields and, if any,
		 ;;store the values of the sub-RTDs fields.
		 (%fill the-record 0 formals ?list-of-field-values-lists))))
	  )))

    (sub-main))

  (define (%make-wrapper-for-protocol/with-parent this-total-number-of-fields prcd protocol)
    (let* ((pprcd			(<rcd>-parent-rcd prcd))
	   (parent-number-of-fields	(<rtd>-size (<rcd>-rtd prcd)))
	   (parent-constructor-wrapper	(%protocol-wrapper parent-number-of-fields pprcd
							   (<rcd>-protocol prcd)))
	   (this-number-of-fields	(fx- this-total-number-of-fields parent-number-of-fields))
	   (the-protocol
	    (or protocol
		(lambda (make-parent-record) ;default protocol function
		  (lambda all-fields
		    (let-values (((parent-fields this-fields)
				  (%split all-fields (fx- (length all-fields) this-number-of-fields))))
		      (apply (apply make-parent-record parent-fields) this-fields)))))))
      ;;This is the constructor wrapper.
      (lambda (list-of-field-values-lists)
	;;Evaluate  the  protocol function  and  return the  constructor
	;;function.
	(the-protocol
	 (lambda parent-initialisator-formals
	   (lambda field-values
	     (arguments-validation-forms
	      (unless (fx=? (length field-values) this-number-of-fields)
		(apply error 'a-record-constructor
		       (string-append "expected " (number->string this-number-of-fields)
				      " arguments, got " (number->string (length field-values))
				      " instead")
		       field-values)))
	     (apply (parent-constructor-wrapper (cons field-values list-of-field-values-lists))
		    parent-initialisator-formals)))))))

  (define (%fill the-record field-index field-values-list list-of-field-values-lists)
    ;;Recursive function.   Fill THE-RECORD  with values from  the given
    ;;lists.  Return THE-RECORD itself.
    ;;
    (if (null? field-values-list)
	(if (null? list-of-field-values-lists)
	    the-record
	  (%fill the-record field-index
		 (car list-of-field-values-lists)
		 (cdr list-of-field-values-lists)))
      (begin
	($struct-set! the-record field-index (car field-values-list))
	(%fill the-record (fx+ 1 field-index) (cdr field-values-list) list-of-field-values-lists))))

  (define (%split all-fields n)
    ;;Split the  list ALL-FIELDS and  return two values: a  list holding
    ;;the first N values from ALL-FIELDS, a list holding the rest.
    ;;
    (let loop ((ls all-fields) (n n))
      (arguments-validation-forms
       (unless (or (zero? n) (pair? ls))
	 (assertion-violation who "insufficient arguments" all-fields)))
      (if (zero? n)
	  (values '() ls)
	(let-values (((m p) (loop (cdr ls) (fx- n 1))))
	  (values (cons (car ls) m) p)))))

  (main))


;;;; record accessors and mutators
;;
;;We have to remember that, given the definitions:
;;
;; (define-record-type <alpha>
;;   (fields a b c))
;;
;; (define-record-type <beta>
;;   (parent <alpha>)
;;   (fields d e f))
;;
;; (define-record-type <gamma>
;;   (parent <beta>)
;;   (fields g h i))
;;
;;the layout of a record of type <GAMMA> is something like:
;;
;;  [RTD-<gamma>, a, b, c, d, e, f, g, h, i]
;;
;;               |0, 1, 2|			relative offsets of <ALPHA>
;;                        |0, 1, 2|		relative offsets of <BETA>
;;                                 |0, 1, 2|	relative offsets of <GAMMA>
;;               |0, 1, 2, 3, 4, 5, 6, 7, 8|	absolute offsets of <GAMMA>
;;

(define (record-accessor rtd relative-field-index)
  ;;Return a function being  the accessor for field RELATIVE-FIELD-INDEX
  ;;of RTD.
  ;;
  (define who 'record-accessor)
  (with-arguments-validation (who)
      ((rtd	rtd)
       (index	relative-field-index))
    (let* ((total-number-of-fields	(<rtd>-size rtd))
	   (prtd			(<rtd>-parent rtd))
	   (abs-index			(if prtd
					    (+ relative-field-index (<rtd>-size prtd))
					  relative-field-index)))
      (with-arguments-validation (who)
      	  ((absolute-field-index abs-index total-number-of-fields rtd relative-field-index))
	(lambda (obj)
	  ;;We  must  verify  that  OBJ  is actually  an  R6RS  record
	  ;;instance of RTD or one of its subtypes.
	  ;;
	  (define who 'a-record-accessor)
	  (with-arguments-validation (who)
	      ((record			obj)
	       (record-instance-of-rtd	obj rtd))
	    ($struct-ref obj abs-index)))))))

(define (record-mutator rtd relative-field-index)
  ;;Return a function being  the mutator for field RELATIVE-FIELD-INDEX
  ;;of RTD.
  ;;
  (define who 'record-mutator)
  (with-arguments-validation (who)
      ((rtd	rtd)
       (index	relative-field-index))
    (let* ((total-number-of-fields	(<rtd>-size rtd))
	   (prtd			(<rtd>-parent rtd))
	   (abs-index			(if prtd
					    (+ relative-field-index (<rtd>-size prtd))
					  relative-field-index)))
      (with-arguments-validation (who)
	  ((absolute-field-index		abs-index total-number-of-fields
						rtd relative-field-index)
	   (relative-index-of-mutable-field	relative-field-index rtd))
	(let ((who (string->symbol (string-append "a-record-mutator/"
						  (symbol->string (<rtd>-name rtd))))))
	  (lambda (obj new-value)
	    ;;We  must  verify  that  OBJ  is actually  an  R6RS  record
	    ;;instance of RTD or one of its subtypes.
	    ;;
	    (with-arguments-validation (who)
		((record			obj)
		 (record-instance-of-rtd	obj rtd))
	      ($struct-set! obj abs-index new-value))))))))


(define (record-predicate rtd)
  ;;Return a function being the predicate for RTD.
  ;;
  (define who 'record-predicate)
  (with-arguments-validation (who)
      ((rtd	rtd))
    (lambda (x)
      ;;We must verify that X is  actually a record instance of RTD or a
      ;;record instance of a subtype of RTD.
      (and ($struct? x)
	   (let ((rtd^ ($struct-rtd x)))
	     (or (eq? rtd rtd^)
		 (and (<rtd>? rtd^)
		      (let upper-parent ((prtd^ (<rtd>-parent rtd^)))
			(and prtd^
			     (or (eq? prtd^ rtd)
				 (upper-parent (<rtd>-parent prtd^))))))))))))


;;;; non-R6RS extensions

(define (rtd-subtype? rtd prtd)
  ;;Return true if PRTD is a parent of RTD or they are equal.
  ;;
  (define who 'rtd-subtype?)
  (with-arguments-validation (who)
      ((rtd	rtd)
       (rtd	prtd))
    (or (eq? rtd prtd)
	(let upper-parent ((prtd^ (<rtd>-parent rtd)))
	  (and prtd^
	       (or (eq? prtd^ prtd)
		   (upper-parent (<rtd>-parent prtd^))))))))


;;;; done

(set-rtd-printer! (type-descriptor <rtd>) %rtd-printer)
(set-rtd-printer! (type-descriptor <rcd>) %rcd-printer)

)

#!eof

rtd0  fields=4
proto0 =
  (lambda (n)
    (lambda (p0-fmls ...)
      (n f0 f1 f2 f3)))

rtd1  fields=2
proto1 =
  (lambda (n)
    (lambda (p1-fmls ...)
      ((n p0-acts ...) f4 f5)))

rtd2  fields=1
proto2 =
  (lambda (n)
    (lambda (p2-fmls ...)
      ((n p1-acts ...) f6)))


(record-constructor rcd2)
==
(proto2 (lambda p1-fml*
          (lambda (f6)
            (apply (proto1 (lambda p0-fml*
                             (lambda (f4 f5)
                               (apply (proto0 (lambda (f0 f1 f2 f3)
                                                ($record rtd2 f0 f1 f2 f3 f4 f5 f6)))
                                      p0-fml*))))
                   p1-fml*))))

new0 = (lambda (f0 f1 f2 f3 f4 f5 f6)
         ($record rtd2 f0 f1 f2 f3 f4 f5 f6))

(record-constructor rcd2)
==
(proto2 (lambda p1-fml*
          (lambda (f6)
            (apply (proto1 (lambda p0-fml*
                             (lambda (f4 f5)
                               (apply (proto0 (lambda (f0 f1 f2 f3)
                                                (new0 f0 f1 f2 f3 f4 f5 f6)))
                                      p0-fml*))))
                   p1-fml*))))


;;; end of file
