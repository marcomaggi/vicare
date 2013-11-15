;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: labels for built-in objects
;;;Date: Tue Aug  6, 2013
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
(library (nausicaa language builtins)
  (export
    <top>
    <boolean> <symbol> <keyword> <pointer>
    <pair> <mutable-pair> <spine> <list> <nonempty-list>
    <char>
    <string> <ascii-string> <latin1-string> <percent-encoded-string> <mutable-string>
    <vector> <record-type-descriptor> <record> <condition>
    <hashtable> <hashtable-eq> <hashtable-eqv> <string-hashtable> <string-ci-hashtable> <symbol-hashtable>
    <fixnum> <positive-fixnum> <negative-fixnum>
    <nonzero-fixnum> <nonpositive-fixnum> <nonnegative-fixnum>
    <flonum>
    <exact-integer> <integer> <integer-valued>
    <rational> <rational-valued>
    <real> <real-valued> <complex> <number>
    <procedure>

    <transcoder> <port>
    <input-port> <output-port> <input/output-port>
    <binary-port> <binary-input-port> <binary-output-port> <binary-input/output-port>
    <textual-port> <textual-input-port> <textual-output-port> <textual-input/output-port>

    <bytevector> <nonempty-bytevector>
    <bytevector-u8> <bytevector-s8>
    <bytevector-u16l> <bytevector-s16l> <bytevector-u16b> <bytevector-s16b> <bytevector-u16n> <bytevector-s16n>
    <bytevector-u32l> <bytevector-s32l> <bytevector-u32b> <bytevector-s32b> <bytevector-u32n> <bytevector-s32n>
    <bytevector-u64l> <bytevector-s64l> <bytevector-u64b> <bytevector-s64b> <bytevector-u64n> <bytevector-s64n>
    ;;<bytevector-uintl> <bytevector-sintl> <bytevector-uintb> <bytevector-sintb> <bytevector-uintn> <bytevector-sintn>
    <bytevector-singlel> <bytevector-singleb> <bytevector-singlen> <bytevector-doublel> <bytevector-doubleb> <bytevector-doublen>
    <ascii-bytevector> <latin1-bytevector> <percent-encoded-bytevector>

    <hashable-and-properties-clauses>

    <unspecified> <undefined>

    ;; multimethods
    define-generic		define-generic*
    tag-unique-identifiers-of	object->string

    ;; multimethods for input ports
    get-single			lookahead-single
    get-multi-n			get-multi-n!
    get-multi-some		get-multi-all

    ;; multimethods for output ports
    put-single			put-multi-2
    put-multi-3			put-multi-4)
  (import (vicare)
    (vicare language-extensions sentinels)
    (nausicaa language oopp)
    (nausicaa language multimethods)
    (vicare unsafe operations)
    ;;FIXME  To be  removed at  the  next boot  image rotation.   (Marco
    ;;Maggi; Sat Oct 26, 2013)
    (only (vicare system $fx)
	  $fixnum->string)
    (only (vicare system $symbols)
	  $symbol-value
	  $set-symbol-value!
	  $unbound-object?)
    (vicare containers bytevectors)
    ;;FIXME  To be  removed at  the  next boot  image rotation.   (Marco
    ;;Maggi; Mon Nov 4, 2013)
    (only (vicare system $bytevectors)
	  $ascii-encoded-bytevector?
	  $latin1-encoded-bytevector?
	  $percent-encoded-bytevector?
	  $percent-encode
	  $percent-decode)
    ;;FIXME  To be  removed at  the  next boot  image rotation.   (Marco
    ;;Maggi; Sat Nov 9, 2013)
    (only (vicare system $strings)
	  $ascii-encoded-string?
	  $latin1-encoded-string?
	  $percent-encoded-string?))


;;;; helpers

(define-syntax (define-builtin-label stx)
  ;;This is  only for internal use  in this library.  Define  a built-in
  ;;label type.
  ;;
  (syntax-case stx ()
    ((_ ?class-name ?clause ...)
     (let ((tag-id #'?class-name))
       (with-syntax ((UID (datum->syntax tag-id
					 (string->symbol
					  (string-append "nausicaa:builtin:"
							 (symbol->string (syntax->datum tag-id)))))))
	 #'(define-label ?class-name
	     (nongenerative UID)
	     ?clause ...))))))


;;;; built-in types: booleans, symbols, keywords, pointers

(define-builtin-label <boolean>
  (predicate boolean?))

(define-builtin-label <symbol>
  (predicate symbol?)
  (virtual-fields (immutable (string <string>)	symbol->string))
  (method-syntax hash
    (syntax-rules ()
      ((_ ?o)
       (symbol-hash ?o)))))

(define-builtin-label <keyword>
  (predicate keyword?)
  (protocol (lambda ()
	      (lambda (arg)
		(cond ((symbol? arg)
		       (symbol->keyword arg))
		      ((string? arg)
		       (symbol->keyword (string->symbol arg)))
		      (else
		       (assertion-violation '<keyword>
			 "expected symbol or string as constructor argument" arg))))))
  (virtual-fields (immutable (string <string>) (lambda/tags ((K <keyword>))
						 (symbol->string (K symbol))))
		  (immutable (symbol <symbol>) keyword->symbol)))

(define-builtin-label <pointer>
  (predicate pointer?)
  (protocol (lambda () integer->pointer))
  (virtual-fields
   (immutable (null?	<boolean>)	pointer-null?)
   (immutable (integer	<integer>)	pointer->integer))

  (method-syntax =
    (syntax-rules ()
      ((_ ?ptr1 ?ptr2)
       (pointer=? ?ptr1 ?ptr2))))

  (method-syntax <>
    (syntax-rules ()
      ((_ ?ptr1 ?ptr2)
       (pointer<>? ?ptr1 ?ptr2))))

  (method-syntax <
    (syntax-rules ()
      ((_ ?ptr1 ?ptr2)
       (pointer<? ?ptr1 ?ptr2))))

  (method-syntax >
    (syntax-rules ()
      ((_ ?ptr1 ?ptr2)
       (pointer>? ?ptr1 ?ptr2))))

  (method-syntax <=
    (syntax-rules ()
      ((_ ?ptr1 ?ptr2)
       (pointer<=? ?ptr1 ?ptr2))))

  (method-syntax >=
    (syntax-rules ()
      ((_ ?ptr1 ?ptr2)
       (pointer>=? ?ptr1 ?ptr2))))

  (method-syntax add
    (syntax-rules ()
      ((_ ?ptr ?offfset)
       (pointer-add ?ptr ?offset))))

  (method-syntax diff
    (syntax-rules ()
      ((_ ?ptr1 ?ptr2)
       (pointer-diff ?ptr1 ?ptr2))))

  (method-syntax clone
    (syntax-rules ()
      ((_ ?ptr)
       (pointer-clone ?ptr))))

  (method-syntax set-null!
    (syntax-rules ()
      ((_ ?ptr)
       (set-pointer-null! ?ptr))))

  #| end of label: <pointer> |# )


;;;; built-in types: pairs and lists

(define-builtin-label <spine>
  ;;A spine is  the head of a proper  list: null or a pair  whose cdr is
  ;;null  or a  pair.  It  can be  used to  iterate over  a proper  list
  ;;without fully validating it first.
  ;;
  (protocol (lambda ()
	      (lambda/tags (A (D <spine>))
		(cons A D))))

  (predicate (lambda (obj)
	       (or (and (pair? obj)
			(or (pair? ($cdr obj))
			    (null? ($cdr obj))))
		   (null? obj))))

  (maker (lambda (stx)
	   (syntax-case stx ()
	     ((_ ())
	      #'(quote ()))
	     ((_ (?car ?cdr))
	      #'(make-<spine> ?car ?cdr))
	     )))

  (virtual-fields (immutable (length	<exact-integer>)	length)
		  (immutable (null?	<boolean>) 		null?)
		  (immutable (pair?	<boolean>)		pair?)
		  (immutable car				car)
		  (immutable (cdr	<spine>)		cdr)
		  (immutable $car				$car)
		  (immutable ($cdr	<spine>)		$cdr))

  ;; --------------------------------------------------------------------

  (method-syntax caar
    (syntax-rules ()
      ((_ o)
       (caar o))))

  (method-syntax cadr
    (syntax-rules ()
      ((_ o)
       (cadr o))))

  (method-syntax cdar
    (syntax-rules ()
      ((_ o)
       (cdar o))))

  (method-syntax cddr
    (syntax-rules ()
      ((_ o)
       (cddr o))))

  (method-syntax caaar
    (syntax-rules ()
      ((_ o)
       (caaar o))))

  (method-syntax caadr
    (syntax-rules ()
      ((_ o)
       (caadr o))))

  (method-syntax cadar
    (syntax-rules ()
      ((_ o)
       (cadar o))))

  (method-syntax caddr
    (syntax-rules ()
      ((_ o)
       (caddr o))))

  (method-syntax cdaar
    (syntax-rules ()
      ((_ o)
       (cdaar o))))

  (method-syntax cdadr
    (syntax-rules ()
      ((_ o)
       (cdadr o))))

  (method-syntax cddar
    (syntax-rules ()
      ((_ o)
       (cddar o))))

  (method-syntax cdddr
    (syntax-rules ()
      ((_ o)
       (cdddr o))))

  ;; --------------------------------------------------------------------

  (method-syntax map
    (syntax-rules ()
      ((_ o proc . lists)
       (map proc o . lists))))

  (method-syntax for-each
    (syntax-rules ()
      ((_ o proc . lists)
       (for-each proc o . lists))))

  (method-syntax find
    (syntax-rules ()
      ((_ o proc)
       (find proc o))))

  (method-syntax reverse
    (syntax-rules ()
      ((_ o)
       (reverse o))))

  (method-syntax for-all
    (syntax-rules ()
      ((_ o proc . lists)
       (for-all proc o . lists))))

  (method-syntax exists
    (syntax-rules ()
      ((_ o proc . lists)
       (exists proc o . lists))))

  (method-syntax filter
    (syntax-rules ()
      ((_ o proc)
       (filter proc o))))

  (method-syntax partition
    (syntax-rules ()
      ((_ o proc)
       (partition proc o))))

  (method-syntax fold-left
    (syntax-rules ()
      ((_ o nil proc)
       (fold-left proc nil o))))

  (method-syntax fold-right
    (syntax-rules ()
      ((_ o nil proc)
       (fold-right proc nil o))))

  (method-syntax remp
    (syntax-rules ()
      ((_ o proc)
       (remp proc o))))

  (method-syntax remove
    (syntax-rules ()
      ((_ o proc)
       (remove proc o))))

  (method-syntax remv
    (syntax-rules ()
      ((_ o proc)
       (remv proc o))))

  (method-syntax remq
    (syntax-rules ()
      ((_ o proc)
       (remq proc o))))

  (method-syntax memp
    (syntax-rules ()
      ((_ o proc)
       (memp proc o))))

  (method-syntax member
    (syntax-rules ()
      ((_ o proc)
       (member proc o))))

  (method-syntax memv
    (syntax-rules ()
      ((_ o proc)
       (memv proc o))))

  (method-syntax memq
    (syntax-rules ()
      ((_ o proc)
       (memq proc o))))

  (method-syntax assp
    (syntax-rules ()
      ((_ o proc)
       (assp proc o))))

  (method-syntax assoc
    (syntax-rules ()
      ((_ o proc)
       (assoc proc o))))

  (method-syntax assv
    (syntax-rules ()
      ((_ o proc)
       (assv proc o))))

  (method-syntax assq
    (syntax-rules ()
      ((_ o proc)
       (assq proc o))))

  #| end of label |# )

;;; --------------------------------------------------------------------

(define-builtin-label <pair>
  (protocol (lambda () cons))
  (predicate pair?)
  (virtual-fields (immutable car car)
		  (immutable cdr cdr)
		  (immutable $car $car)
		  (immutable $cdr $cdr)))

(define-builtin-label <mutable-pair>
  (parent <pair>)
  (protocol (lambda () cons))
  (predicate pair?)
  (virtual-fields (mutable car car set-car!)
		  (mutable cdr cdr set-cdr!)
		  (mutable $car $car $set-car!)
		  (mutable $cdr $cdr $set-cdr!)))

;;; --------------------------------------------------------------------

(define-builtin-label <list>
  (parent <spine>)
  (protocol (lambda () list))
  (predicate list?))

(define-builtin-label <nonempty-list>
  (parent <list>)
  (protocol (lambda () list))
  (predicate pair?))


;;;; built-in types: arrays common stuff

(define-mixin <array>
  (virtual-fields
   (immutable (empty?    <boolean>)	(lambda/tags ((S <array>))
					  ($fxzero? (S length))))
   (immutable ($empty?   <boolean>)	(lambda/tags ((S <array>))
					  ($fxzero? (S $length)))))

  (method (%normalise-index (S <array>) (idx <fixnum>))
    ;;This is private.  Non-negative indexes are handled as specified by
    ;;R6RS; negative  indexes are handled as  counts from the end  o the
    ;;array.
    ;;
    (define who '%normalise-index)
    (cond ((or ($fxzero? idx)
	       (and ($fxpositive? idx)
		    ($fx< idx (S $length))))
	   idx)
	  ((and ($fxnegative? idx)
		($fx< ($fx- idx) (S $length)))
	   ($fx+ idx (S $length)))
	  (else
	   (assertion-violation who "array index out of range" idx))))

  (getter (lambda (stx)
	    (syntax-case stx ()
	      ((?var ((?idx)))
	       #'(?var ref ?idx))))))

(define-mixin <mutable-array>
  (setter (lambda (stx)
	    (syntax-case stx ()
	      ((?var ((?index)) ?val)
	       #'(?var set! ?index ?val))))))


;;;; built-in types: characters and strings

(define-builtin-label <char>
  (predicate char?)
  (virtual-fields (immutable (upcase    <char>)	char-upcase)
		  (immutable (downcase  <char>)	char-downcase)
		  (immutable (titlecase <char>)	char-titlecase)
		  (immutable (foldcase  <char>)	char-foldcase)

		  (immutable (alphabetic? <boolean>)	char-alphabetic?)
		  (immutable (numeric?    <boolean>)	char-numeric?)
		  (immutable (whitespace? <boolean>)	char-whitespace?)
		  (immutable (upper-case? <boolean>)	char-upper-case?)
		  (immutable (lower-case? <boolean>)	char-lower-case?)
		  (immutable (title-case? <boolean>)	char-title-case?)

		  (immutable (general-category <symbol>) char-general-category)))

;;; --------------------------------------------------------------------

(define-builtin-label <string>
  (mixins <array>)
  (protocol (lambda () string))
  (predicate string?)
  (virtual-fields
   (immutable (length		<fixnum>)			string-length)
   (immutable ($length		<fixnum>)			$string-length)
   (immutable (upcase		<string>)			string-upcase)
   (immutable (downcase		<string>)			string-downcase)
   (immutable (titlecase	<string>)			string-titlecase)
   (immutable (foldcase		<string>)			string-foldcase)

   (immutable (ascii		<ascii-bytevector>)		string->ascii)
   (immutable (latin1		<bytevector-u8>)		string->latin1)
   (immutable (utf8		<bytevector-u8>)		string->utf8)
   (immutable (utf16		<bytevector>)			string->utf16)
   (immutable (utf16le		<bytevector-u16l>)		string->utf16le)
   (immutable (utf16be		<bytevector-u16b>)		string->utf16be)
   (immutable (utf16n		<bytevector-u16n>)		string->utf16n)
   (immutable (utf32		<bytevector-u32>)		string->utf32)
   (immutable (percent-encoding	<percent-encoded-bytevector>)	string->uri-encoding)

   (immutable ($ascii		<ascii-bytevector>)		$string->ascii)
   (immutable ($latin1		<bytevector-u8>)		$string->latin1)

   #| end of virtual-fields |#)

  (method (ref (S <string>) (idx <fixnum>))
    ($string-ref S (S %normalise-index idx)))

  (method-syntax hash
    (syntax-rules ()
      ((_ ?o)
       (string-hash ?o))))

  (method substring
    (case-lambda/tags
     (((S <string>) (start <fixnum>))
      (substring S
		 (S %normalise-index start)
		 (S $length)))
     (((S <string>) (start <fixnum>) (end <fixnum>))
      (substring S
		 (S %normalise-index start)
		 (S %normalise-index end)))))

  (method-syntax append
    (syntax-rules ()
      ((_ ?o . ?strings)
       (string-append ?o . ?strings))))

  (method-syntax list
    (syntax-rules ()
      ((_ ?o)
       (string->list ?o))))

  (method-syntax for-each
    (syntax-rules ()
      ((_ ?o ?proc)
       (string-for-each ?proc ?o))))

  (method-syntax copy
    (syntax-rules ()
      ((_ ?o)
       (string-copy ?o)))))

(define-builtin-label <mutable-string>
  (parent <string>)
  (mixins <mutable-array>)
  (protocol (lambda () string))
  (predicate string?)
  (method (set! (S <mutable-string>) (idx <fixnum>) (ch <char>))
    ($string-set! S (S %normalise-index idx) ch)))

(define-builtin-label <percent-encoded-string>
  (parent <string>)
  (protocol (lambda () string))
  (predicate $percent-encoded-string?))

(define-builtin-label <ascii-string>
  (parent <string>)
  (protocol (lambda () string))
  (predicate $ascii-encoded-string?))

(define-builtin-label <latin1-string>
  (parent <string>)
  (protocol (lambda () string))
  (predicate $latin1-encoded-string?))


;;;; built-in types: vector

(define-builtin-label <vector>
  (mixins <array> <mutable-array>)
  (protocol (lambda () vector))
  (predicate vector?)
  (virtual-fields (immutable (length	<fixnum>)	vector-length)
		  (immutable ($length	<fixnum>)	$vector-length))

;;; --------------------------------------------------------------------

  (method (ref (S <vector>) (idx <fixnum>))
    ($vector-ref S (S %normalise-index idx)))

  (method (set! (S <vector>) (idx <fixnum>) item)
    ($vector-set! S (S %normalise-index idx) item))

;;; --------------------------------------------------------------------

  (method subvector
    (case-lambda/tags
     (((S <vector>) (start <fixnum>))
      (subvector S
		 (S %normalise-index start)
		 (S $length)))
     (((S <vector>) (start <fixnum>) (end <fixnum>))
      (subvector S
		 (S %normalise-index start)
		 (S %normalise-index end)))))

  (method-syntax map
    (syntax-rules ()
      ((_ o proc . vectors)
       (vector-map proc o . vectors))))

  (method-syntax for-each
    (syntax-rules ()
      ((_ o proc . vectors)
       (vector-for-each proc o . vectors))))

  (method-syntax for-all
    (syntax-rules ()
      ((_ o proc . vectors)
       (vector-for-all proc o . vectors))))

  (method-syntax exists
    (syntax-rules ()
      ((_ o proc . vectors)
       (vector-exists proc o . vectors))))

  (method-syntax fill!
    (syntax-rules ()
      ((_ o fill)
       (vector-fill! o fill))))

  #| end of label |# )


;;;; built-in types: bytevectors

(define-builtin-label <bytevector>
  (predicate bytevector?)

  (virtual-fields
   (immutable (length			<fixnum>)			bytevector-length)
   (immutable ($length			<fixnum>)			$bytevector-length)

   (immutable (percent-encoded		<percent-encoded-bytevector>)	percent-encode)
   (immutable (percent-decoded		<bytevector>)			percent-decode)
   (immutable ($percent-encoded		<percent-encoded-bytevector>)	$percent-encode)
   (immutable ($percent-decoded		<bytevector>)			$percent-decode)
   (immutable (percent-encoded?		<boolean>)			percent-encoded-bytevector?)
   (immutable ($percent-encoded?	<boolean>)			$percent-encoded-bytevector?)

   (immutable (ascii-encoded?		<boolean>)			ascii-encoded-bytevector?)
   (immutable ($ascii-encoded?		<boolean>)			$ascii-encoded-bytevector?)

   (immutable (latin1-encoded?		<boolean>)			latin1-encoded-bytevector?)
   (immutable ($latin1-encoded?		<boolean>)			$latin1-encoded-bytevector?)

   #| end of virtual-fields|# )

  (method-syntax copy
    (syntax-rules ()
      ((_ ?bv)
       (bytevector-copy ?bv))))

  (method-syntax hash
    (syntax-rules ()
      ((_ ?bv)
       (bytevector-hash ?bv))))

  (method-syntax $hash
    (syntax-rules ()
      ((_ ?bv)
       ($bytevector-hash ?bv))))

  #| end of label |# )

(define-label <nonempty-bytevector>
  (parent <bytevector>)
  (predicate (lambda (bv)
	       ($fxpositive? ($bytevector-length bv)))))

(let-syntax
    ((define-bytevector-label
       (lambda (stx)
	 (syntax-case stx ()
	   ((_ ?type ?setter-getter-stem)
	    (let ((setter-getter-stem (syntax->datum #'?setter-getter-stem)))
	      (define (%mkid suffix.str)
		(datum->syntax #'?type (string->symbol
					(string-append "bytevector-"
						       setter-getter-stem
						       suffix.str))))
	      (with-syntax
		  ((SETTER (%mkid "-scaled-set!"))
		   (GETTER (%mkid "-scaled-ref")))
		#'(define-builtin-label ?type
		    (parent <bytevector>)
		    (setter (lambda (stx)
			      (syntax-case stx ()
				((?var ((?idx)) ?val)
				 #'(SETTER ?var ?idx ?val)))))
		    (getter (lambda (stx)
			      (syntax-case stx ()
				((?var ((?idx)))
				 #'(GETTER ?var ?idx))))))
		)))
	   ))))
  (define-bytevector-label <bytevector-u8>	"u8-native")
  (define-bytevector-label <bytevector-s8>	"s8-native")

  (define-bytevector-label <bytevector-u16l>	"u16-litend")
  (define-bytevector-label <bytevector-s16l>	"s16-litend")
  (define-bytevector-label <bytevector-u16b>	"u16-bigend")
  (define-bytevector-label <bytevector-s16b>	"s16-bigend")
  (define-bytevector-label <bytevector-u16n>	"u16-native")
  (define-bytevector-label <bytevector-s16n>	"s16-native")

  (define-bytevector-label <bytevector-u32l>	"u32-litend")
  (define-bytevector-label <bytevector-s32l>	"s32-litend")
  (define-bytevector-label <bytevector-u32b>	"u32-bigend")
  (define-bytevector-label <bytevector-s32b>	"s32-bigend")
  (define-bytevector-label <bytevector-u32n>	"u32-native")
  (define-bytevector-label <bytevector-s32n>	"s32-native")

  (define-bytevector-label <bytevector-u64l>	"u64-litend")
  (define-bytevector-label <bytevector-s64l>	"s64-litend")
  (define-bytevector-label <bytevector-u64b>	"u64-bigend")
  (define-bytevector-label <bytevector-s64b>	"s64-bigend")
  (define-bytevector-label <bytevector-u64n>	"u64-native")
  (define-bytevector-label <bytevector-s64n>	"s64-native")

  ;; (define-bytevector-label <bytevector-uintl>	"uint-litend")
  ;; (define-bytevector-label <bytevector-sintl>	"sint-litend")
  ;; (define-bytevector-label <bytevector-uintb>	"uint-bigend")
  ;; (define-bytevector-label <bytevector-sintb>	"sint-bigend")
  ;; (define-bytevector-label <bytevector-uintn>	"uint-native")
  ;; (define-bytevector-label <bytevector-sintn>	"sint-native")

  (define-bytevector-label <bytevector-singlel>	"ieee-single-litend")
  (define-bytevector-label <bytevector-singleb>	"ieee-single-bigend")
  (define-bytevector-label <bytevector-singlen>	"ieee-single-native")

  (define-bytevector-label <bytevector-doublel>	"ieee-double-litend")
  (define-bytevector-label <bytevector-doubleb>	"ieee-double-bigend")
  (define-bytevector-label <bytevector-doublen>	"ieee-double-native")

  #| end of let-syntax |# )

(define-label <ascii-bytevector>
  (parent <bytevector-u8>)
  (predicate $ascii-encoded-bytevector?))

(define-label <latin1-bytevector>
  (parent <bytevector-u8>)
  (predicate $latin1-encoded-bytevector?))

(define-label <percent-encoded-bytevector>
  (parent <bytevector-u8>)
  (predicate $percent-encoded-bytevector?))


;;;; built-in types: hashtables

(define-builtin-label <hashtable>
  (predicate hashtable?)
  (virtual-fields (immutable size hashtable-size)
		  (immutable keys hashtable-keys)
		  (immutable entries hashtable-entries)
		  (immutable (mutable? <boolean>) hashtable-mutable?))
  (getter (lambda (stx)
	    (syntax-case stx ()
	      ((?var ((?key)))
	       #'(hashtable-ref ?var ?key (void)))
	      ((?var ((?key) (?default)))
	       #'(hashtable-ref ?var ?key ?default))
	      )))

  (setter (lambda (stx)
	    (syntax-case stx ()
	      ((?var ((?index)) ?val)
	       #'(hashtable-set! ?var ?index ?val)))))

  (method-syntax delete!
    (syntax-rules ()
      ((_ ?table ?key)
       (hashtable-delete! ?table ?key))))

  (method-syntax contains?
    (syntax-rules ()
      ((_ ?table ?key)
       (hashtable-contains? ?table ?key))))

  (method-syntax clear!
    (syntax-rules ()
      ((_ ?table)
       (hashtable-clear! ?table))))

  (method-syntax copy
    (syntax-rules ()
      ((_ ?table)
       (hashtable-copy ?table))
      ((_ ?table ?mutable)
       (hashtable-copy ?table ?mutable))))

  (method-syntax update!
    (syntax-rules ()
      ((_ ?table ?key ?proc ?default)
       (hashtable-update! ?table ?key ?proc ?default))))

  #| end of label |# )

;;; --------------------------------------------------------------------

(define-builtin-label <hashtable-eq>
  (parent <hashtable>)

  (protocol (lambda ()
	      (lambda entries
		(receive-and-return (table)
		    (make-eq-hashtable)
		  (for-each (lambda (entry)
			      (hashtable-set! table (car entry) (cdr entry)))
		    entries)))))

  (maker (lambda (stx)
	   (syntax-case stx ()
	     ((_ ((?key ?val) ...))
	      #'(make-<hashtable-eq> (cons ?key ?val) ...))
	     ((_ ((?key . ?val) ...))
	      #'(make-<hashtable-eq> (cons ?key ?val) ...))
	     )))

  #| end of label |# )

;;; --------------------------------------------------------------------

(define-builtin-label <hashtable-eqv>
  (parent <hashtable>)
  (protocol (lambda ()
	      (lambda entries
		(receive-and-return (table)
		    (make-eqv-hashtable)
		  (for-each (lambda (entry)
			      (hashtable-set! table (car entry) (cdr entry)))
		    entries)))))
  (maker (lambda (stx)
	   (syntax-case stx ()
	     ((_ ((?key ?val) ...))
	      #'(make-<hashtable-eqv> (cons ?key ?val) ...))
	     ((_ ((?key . ?val) ...))
	      #'(make-<hashtable-eqv> (cons ?key ?val) ...))
	     )))
  #| end of label |# )

;;; --------------------------------------------------------------------

(define-builtin-label <string-hashtable>
  (parent <hashtable>)
  (protocol
   (lambda ()
     (lambda entries
       (receive-and-return (table)
	   (make-hashtable string-hash string=?)
	 (for-each (lambda (entry)
		     (hashtable-set! table (car entry) (cdr entry)))
	   entries)))))
  (maker
   (lambda (stx)
     (syntax-case stx ()
       ((_ ((?key ?val) ...))
	#'(make-<string-hashtable> (cons ?key ?val) ...))
       ((_ ((?key . ?val) ...))
	#'(make-<string-hashtable> (cons ?key ?val) ...))
       ))))

;;; --------------------------------------------------------------------

(define-builtin-label <string-ci-hashtable>
  (parent <hashtable>)
  (protocol
   (lambda ()
     (lambda entries
       (receive-and-return (table)
	   (make-hashtable string-ci-hash string-ci=?)
	 (for-each (lambda (entry)
		     (hashtable-set! table (car entry) (cdr entry)))
	   entries)))))
  (maker
   (lambda (stx)
     (syntax-case stx ()
       ((_ ((?key ?val) ...))
	#'(make-<string-ci-hashtable> (cons ?key ?val) ...))
       ((_ ((?key . ?val) ...))
	#'(make-<string-ci-hashtable> (cons ?key ?val) ...))
       ))))

;;; --------------------------------------------------------------------

(define-builtin-label <symbol-hashtable>
  (parent <hashtable>)
  (protocol
   (lambda ()
     (lambda entries
       (receive-and-return (table)
	   (make-hashtable symbol-hash eq?)
	 (for-each (lambda (entry)
		     (hashtable-set! table (car entry) (cdr entry)))
	   entries)))))
  (maker
   (lambda (stx)
     (syntax-case stx ()
       ((_ ((?key ?val) ...))
	#'(make-<symbol-hashtable> (cons ?key ?val) ...))
       ((_ ((?key . ?val) ...))
	#'(make-<symbol-hashtable> (cons ?key ?val) ...))
       ))))


;;;; built-in types: records and record type descriptors

(define-builtin-label <record-type-descriptor>
  (predicate record-type-descriptor?)
  (virtual-fields (immutable (name		<symbol>)			record-type-name)
		  (immutable (parent		<record-type-descriptor>)	record-type-parent)
		  (immutable (uid		<symbol>)			record-type-uid)
		  (immutable (generative?	<boolean>)			record-type-generative?)
		  (immutable (sealed?		<boolean>)			record-type-sealed?)
		  (immutable (opaque?		<boolean>)			record-type-opaque?)
		  (immutable (field-names	<vector>)			record-type-field-names))

  (method-syntax predicate
    (syntax-rules ()
      ((_ ?rtd)
       (record-predicate ?rtd))))

  (method-syntax accessor
    (syntax-rules ()
      ((_ ?rtd ?field-idx)
       (record-accessor ?rtd ?field-idx))))

  (method-syntax mutator
    (syntax-rules ()
      ((_ ?rtd ?field-idx)
       (record-mutator ?rtd ?field-idx))))

  (method-syntax field-mutable?
    (syntax-rules ()
      ((_ ?rtd ?field-idx)
       (record-field-mutable? ?rtd ?field-idx))))

  #| end of label |# )

(define-builtin-label <record>
  (predicate record?)
  (virtual-fields (immutable (rtd <record-type-descriptor>) record-rtd)))


;;;; built-in types: condition objects

(define-builtin-label <condition>
  (parent <record>)
  (protocol (lambda () condition))
  (predicate condition?)
  (method-syntax simple
    (syntax-rules ()
      ((_ ?cond)
       (simple-conditions ?cond)))))


;;;; built-in types: transcoder objects

(define-builtin-label <transcoder>
  (predicate transcoder?)
  (protocol (lambda () make-transcoder))
  (maker (lambda (stx)
	   (syntax-case stx ()
	     ((_ ?codec)
	      #'(make-<transcoder> ?codec))
	     ((_ ?codec ?eol-style)
	      #'(make-<transcoder> ?codec ?eol-style))
	     ((_ ?codec ?eol-style ?error-handling-mode)
	      #'(make-<transcoder> ?codec ?eol-style ?error-handling-mode))
	     )))
  (virtual-fields (immutable codec transcoder-codec)
		  (immutable (eol-style <symbol>) transcoder-eol-style)
		  (immutable (error-handling-mode <symbol>) transcoder-error-handling-mode)))


;;;; built-in types: port objects

(define-builtin-label <port>
  (predicate port?)
  (virtual-fields
   (immutable (transcoder		<transcoder>)	port-transcoder)

   (immutable (textual?			<boolean>)	textual-port?)
   (immutable (binary?			<boolean>)	binary-port?)
   (immutable (input?			<boolean>)	input-port?)
   (immutable (output?			<boolean>)	output-port?)

   (immutable (has-port-position?	<boolean>)	port-has-port-position?)
   (immutable (has-set-port-position?	<boolean>)	port-has-set-port-position!?)
   (mutable   (port-position		<integer>)	port-position set-port-position!)

   (immutable (closed?			<boolean>)	port-closed?)

   (immutable fd					port-fd)
   (immutable (id			<string>)	port-id)
   (immutable (uid			<symbol>)	port-uid)
   (immutable (hash			<integer>)	port-hash)

   (mutable   (non-blocking-mode?	<boolean>)
	      port-in-non-blocking-mode?
	      (lambda (port bool)
		(if bool
		    (port-set-non-blocking-mode! port)
		  (port-unset-non-blocking-mode! port))))
   ))

;;; --------------------------------------------------------------------

(define-builtin-label <binary-port>
  (parent <port>)
  (predicate binary-port?))

(define-builtin-label <textual-port>
  (parent <port>)
  (predicate textual-port?))

;;; --------------------------------------------------------------------

(module (<input-port>
	 <output-port>
	 <input/output-port>)

  (define-mixin <input-port-clauses>
    (virtual-fields
     (immutable (eof? <boolean>) port-eof?))
    (methods (get-single	get-single)
	     (lookahead-single	lookahead-single)
	     (get-multi-n	get-multi-n)
	     (get-multi-n!	get-multi-n!)
	     (get-multi-some	get-multi-some)
	     (get-multi-all	get-multi-all)))

  (define-mixin <output-port-clauses>
    (virtual-fields
     (mutable (buffer-mode <symbol>) output-port-buffer-mode	set-port-buffer-mode!))
    (method-syntax flush
      (syntax-rules ()
	((_ ?port)
	 (flush-output-port ?port))))
    (methods (put-single put-single)
	     (put-multi  put-multi)))

  ;; ------------------------------------------------------------

  (define-builtin-label <input-port>
    (parent <port>)
    (predicate input-port?)
    (mixins <input-port-clauses>))

  (define-builtin-label <output-port>
    (parent <port>)
    (predicate output-port?)
    (mixins <output-port-clauses>))

  (define-builtin-label <input/output-port>
    (parent <port>)
    (predicate input/output-port?)
    (mixins <input-port-clauses>
	    <output-port-clauses>))

  #| end of module |# )

;;; --------------------------------------------------------------------

(define-builtin-label <binary-input-port>
  (parent <input-port>)
  (predicate binary-port?))

(define-builtin-label <binary-output-port>
  (parent <output-port>)
  (predicate binary-port?))

(define-builtin-label <binary-input/output-port>
  (parent <input/output-port>)
  (predicate binary-port?))

;;; --------------------------------------------------------------------

(module (<textual-input-port>
	 <textual-output-port>
	 <textual-input/output-port>)

  (define-mixin <textual-input-clauses>
    (method-syntax get-datum
      (syntax-rules ()
	((_ ?port)
	 (get-datum ?port))))

    (method-syntax get-line
      (syntax-rules ()
	((_ ?port)
	 (get-line ?port)))))

  (define-mixin <textual-output-clauses>
    (method-syntax put-datum
      (syntax-rules ()
	((_ ?port ?datum)
	 (put-datum ?port ?datum))))

    (method-syntax write
      (syntax-rules ()
	((_ ?port ?datum)
	 (write ?datum ?port))))

    (method-syntax display
      (syntax-rules ()
	((_ ?port ?datum)
	 (display ?datum ?port))))

    (method-syntax pretty-print
      (syntax-rules ()
	((_ ?port ?datum)
	 (pretty-print ?datum ?port)))))

  ;; ------------------------------------------------------------

  (define-builtin-label <textual-input-port>
    (parent <input-port>)
    (predicate textual-port?)
    (mixins <textual-input-clauses>))

  (define-builtin-label <textual-output-port>
    (parent <output-port>)
    (predicate textual-port?)
    (mixins <textual-output-clauses>))

  (define-builtin-label <textual-input/output-port>
    (parent <input/output-port>)
    (predicate textual-port?)
    (mixins <textual-input-clauses>
	    <textual-output-clauses>))

  #| end of module |# )


;;;; built-in types: number objects

(define-builtin-label <number>
  (predicate number?)

  ;;predicates
  (virtual-fields
   (immutable (zero?		<boolean>)	zero?)
   (immutable (finite?		<boolean>)	finite?)
   (immutable (infinite?	<boolean>)	infinite?)
   (immutable (nan?		<boolean>)	nan?))

  ;; exactness
  (virtual-fields
   (immutable (exact?		<boolean>)	exact?)
   (immutable (inexact?		<boolean>)	inexact?))

  ;; conversion
  (virtual-fields
   (immutable (string		<string>)	number->string))

  ;;This method supports the optional arguments of NUMBER->STRING.
  (methods (string-radix number->string))

;;; math functions from (rnrs base (6)) and (vicare)

  ;; arithmetic
  (method-syntax +
    (syntax-rules ()
      ((_ ?num . ?nums)
       (+ ?num . ?nums))))

  (method-syntax -
    (syntax-rules ()
      ((_ ?num . ?nums)
       (- ?num . ?nums))))

  (method-syntax *
    (syntax-rules ()
      ((_ ?num . ?nums)
       (* ?num . ?nums))))

  (method-syntax /
    (syntax-rules ()
      ((_ ?num . ?nums)
       (/ ?num . ?nums))))

  ;; exactness
  (method-syntax exact
    (syntax-rules ()
      ((_ ?num)
       (exact ?num))))

  (method-syntax inexact
    (syntax-rules ()
      ((_ ?num)
       (inexact ?num))))

  ;; powers
  (method-syntax expt
    (syntax-rules ()
      ((_ ?num ?exp)
       (expt ?num ?exp))))

  (method-syntax square
    (syntax-rules ()
      ((_ ?num)
       (square ?num))))

  (method-syntax cube
    (syntax-rules ()
      ((_ ?num)
       (cube ?num))))

  (method-syntax sqrt
    (syntax-rules ()
      ((_ ?num)
       (sqrt ?num))))

  ;; exponentiation and logarithms
  (method-syntax exp
    (syntax-rules ()
      ((_ ?num)
       (exp ?num))))

  (method-syntax log
    (syntax-rules ()
      ((_ ?num)
       (log ?num))
      ((_ ?num ?base)
       (log ?num ?base))
      ))

  ;; trigonometric functions
  (method-syntax sin
    (syntax-rules ()
      ((_ ?num)
       (sin ?num))))

  (method-syntax cos
    (syntax-rules ()
      ((_ ?num)
       (cos ?num))))

  (method-syntax tan
    (syntax-rules ()
      ((_ ?num)
       (tan ?num))))

  (method-syntax asin
    (syntax-rules ()
      ((_ ?num)
       (asin ?num))))

  (method-syntax acos
    (syntax-rules ()
      ((_ ?num)
       (acos ?num))))

  (method-syntax atan
    (syntax-rules ()
      ((_ ?num)
       (atan ?num))))

  ;; hyperbolic functions
  (method-syntax sinh
    (syntax-rules ()
      ((_ ?num)
       (sinh ?num))))

  (method-syntax cosh
    (syntax-rules ()
      ((_ ?num)
       (cosh ?num))))

  (method-syntax tanh
    (syntax-rules ()
      ((_ ?num)
       (tanh ?num))))

  (method-syntax asinh
    (syntax-rules ()
      ((_ ?num)
       (asinh ?num))))

  (method-syntax acosh
    (syntax-rules ()
      ((_ ?num)
       (acosh ?num))))

  (method-syntax atanh
    (syntax-rules ()
      ((_ ?num)
       (atanh ?num))))

  ;; complex numbers typical operations
  (method-syntax conjugate
    (syntax-rules ()
      ((_ ?num)
       (complex-conjugate ?num))))

  (method-syntax real-part
    (syntax-rules ()
      ((_ ?num)
       (real-part ?num))))

  (method-syntax imag-part
    (syntax-rules ()
      ((_ ?num)
       (imag-part ?num))))

  (method-syntax magnitude
    (syntax-rules ()
      ((_ ?num)
       (magnitude ?num))))

  (method-syntax angle
    (syntax-rules ()
      ((_ ?num)
       (angle ?num)))))

;;; --------------------------------------------------------------------

(define-builtin-label <complex>
  (parent <number>)
  (predicate complex?))

;;; --------------------------------------------------------------------

(define-builtin-label <real-valued>
  (parent <complex>)
  (predicate real-valued?)
  (virtual-fields (immutable (positive?		<boolean>)	positive?)
		  (immutable (negative?		<boolean>)	negative?)
		  (immutable (non-positive?	<boolean>)	non-positive?)
		  (immutable (non-negative?	<boolean>)	non-negative?)
		  (immutable (sign		<fixnum>)	sign))

  ;; rational numbers typical operations
  (virtual-fields
   (immutable (numerator	<real-valued>)	numerator)
   (immutable (denominator	<real-valued>)	denominator))

  ;; methods: rounding
  (method-syntax floor
    (syntax-rules ()
      ((_ ?num)
       (floor ?num))))

  (method-syntax ceiling
    (syntax-rules ()
      ((_ ?num)
       (ceiling ?num))))

  (method-syntax truncate
    (syntax-rules ()
      ((_ ?num)
       (truncate ?num))))

  (method-syntax round
    (syntax-rules ()
      ((_ ?num)
       (round ?num))))

  (method-syntax rationalize
    (syntax-rules ()
      ((_ ?num ?tolerance)
       (rationalize ?num ?tolerance)))))

;;; --------------------------------------------------------------------

(define-builtin-label <real>
  (parent <real-valued>)
  (predicate real?)
  (virtual-fields (immutable (abs	<real>)		abs)))

;;; --------------------------------------------------------------------

(define-builtin-label <rational-valued>
  (parent <real>)
  (predicate rational-valued?))

;;; --------------------------------------------------------------------

(define-builtin-label <rational>
  (parent <rational-valued>)
  (predicate rational?))

;;; --------------------------------------------------------------------

(define-builtin-label <integer-valued>
  (parent <rational-valued>)
  (predicate integer-valued?))

;;; --------------------------------------------------------------------

(define-builtin-label <integer>
  (parent <integer-valued>)
  (predicate integer?)
  (virtual-fields
   (immutable (odd?	<boolean>)	odd?)
   (immutable (even?	<boolean>)	even?)))

;;; --------------------------------------------------------------------

(define-builtin-label <exact-integer>
  (parent <integer>)
  (predicate exact-integer?))

;;; --------------------------------------------------------------------

(define-builtin-label <fixnum>
  (parent <exact-integer>)
  (predicate fixnum?)

  (virtual-fields
   (immutable (even?		<boolean>)	fxeven?)
   (immutable (odd?		<boolean>)	fxodd?)
   (immutable (negative?	<boolean>)	fxnegative?)
   (immutable (positive?	<boolean>)	fxpositive?)
   (immutable (non-negative?	<boolean>)	fxnonnegative?)
   (immutable (non-positive?	<boolean>)	fxnonpositive?)
   (immutable (zero?		<boolean>)	fxzero?)
   (immutable (sign		<fixnum>)	fxsign)

   (immutable ($even?		<boolean>)	$fxeven?)
   (immutable ($odd?		<boolean>)	$fxodd?)
   (immutable ($negative?	<boolean>)	$fxnegative?)
   (immutable ($positive?	<boolean>)	$fxpositive?)
   (immutable ($non-negative?	<boolean>)	$fxnonnegative?)
   (immutable ($non-positive?	<boolean>)	$fxnonpositive?)
   (immutable ($zero?		<boolean>)	$fxzero?)
   (immutable ($sign		<fixnum>)	$fxsign)
   #| end of virtual-fields |# )

  ;; methods: conversion
  (method-syntax string
    (syntax-rules ()
      ((_ ?fx)
       (fixnum->string ?fx))
      ((_ ?fx ?base)
       (fixnum->string ?fx ?base))
      ))

  (method-syntax $string
    (syntax-rules ()
      ((_ ?fx)
       ($fixnum->string ?fx 10))
      ((_ ?fx ?base)
       ($fixnum->string ?fx ?base))
      ))

  (method-syntax flonum
    (syntax-rules ()
      ((_ ?fx)
       (fixnum->flonum ?fx))))

  (method-syntax $flonum
    (syntax-rules ()
      ((_ ?fx)
       ($fixnum->flonum ?fx))))

  ;; methods: arithmetic operations
  (method-syntax abs
    (syntax-rules ()
      ((_ ?fx)
       (fxabs ?fx))))

  (method-syntax *
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fx* ?fx1 ?fx2))))

  (method-syntax mul-with-carry
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx3)
       (fx*/carry ?fx1 ?fx2 ?fx3))))

  (method-syntax +
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fx+ ?fx1 ?fx2))))

  (method-syntax add-with-carry
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx3)
       (fx+/carry ?fx1 ?fx2 ?fx3))))

  (method-syntax add1
    (syntax-rules ()
      ((_ ?fx)
       (fx+ ?fx 1))))

  (method-syntax $add1
    (syntax-rules ()
      ((_ ?fx)
       ($fxadd1 ?fx))))

  (method-syntax -
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fx- ?fx1 ?fx2))))

  (method-syntax sub-with-carry
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx3)
       (fx-/carry ?fx1 ?fx2 ?fx3))))

  (method-syntax sub1
    (syntax-rules ()
      ((_ ?fx)
       (fx- ?fx 1))))

  (method-syntax $sub1
    (syntax-rules ()
      ((_ ?fx)
       ($fxsub1 ?fx))))

  (method-syntax div
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fxdiv ?fx1 ?fx2))))

  (method-syntax mod
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fxmod ?fx1 ?fx2))))

  (method-syntax div-and-mod
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fxdiv-and-mod ?fx1 ?fx2))))

  (method-syntax div0
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fxdiv0 ?fx1 ?fx2))))

  (method-syntax mod0
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fxmod0 ?fx1 ?fx2))))

  (method-syntax div0-and-mod0
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fxdiv0-and-mod0 ?fx1 ?fx2))))

  ;; methods: comparison operations
  (method-syntax =
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx ...)
       (fx=? ?fx1 ?fx2 ?fx ...))))

  (method-syntax <
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx ...)
       (fx<? ?fx1 ?fx2 ?fx ...))))

  (method-syntax >
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx ...)
       (fx>? ?fx1 ?fx2 ?fx ...))))

  (method-syntax <=
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx ...)
       (fx<=? ?fx1 ?fx2 ?fx ...))))

  (method-syntax >=
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx ...)
       (fx>=? ?fx1 ?fx2 ?fx ...))))

  ;; methods: logic operations
  (method-syntax and
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx ...)
       (fxand ?fx1 ?fx2 ?fx ...))))

  (method-syntax ior
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx ...)
       (fxior ?fx1 ?fx2 ?fx ...))))

  (method-syntax xor
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx ...)
       (fxxor ?fx1 ?fx2 ?fx ...))))

  (method-syntax not
    (syntax-rules ()
      ((_ ?fx)
       (fxnot ?fx))))

  ;; methods: shift operations
  (method-syntax arithmetic-shift
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fxarithmetic-shift ?fx1 ?fx2))))

  (method-syntax arithmetic-shift-left
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fxarithmetic-shift-left ?fx1 ?fx2))))

  (method-syntax arithmetic-shift-right
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fxarithmetic-shift-right ?fx1 ?fx2))))

  ;; methods: bitwise operations
  (method-syntax bit-set?
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fxbit-set? ?fx1 ?fx2))))

  (method-syntax bit-count
    (syntax-rules ()
      ((_ ?fx)
       (fxbit-count ?fx))))

  (method-syntax bit-field
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx3)
       (fxbit-field ?fx1 ?fx2 ?fx3))))

  (method-syntax copy-bit
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx3)
       (fxcopy-bit ?fx1 ?fx2 ?fx3))))

  (method-syntax copy-bit-field
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx3 ?fx4)
       (fxcopy-bit-field ?fx1 ?fx2 ?fx3 ?fx4))))

  (method-syntax first-bit-set
    (syntax-rules ()
      ((_ ?fx)
       (fxfirst-bit-set ?fx))))

  (method-syntax if
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx3)
       (fxif ?fx1 ?fx2 ?fx3))))

  (method-syntax length
    (syntax-rules ()
      ((_ ?fx)
       (fxlength ?fx))))

  (method-syntax reverse-bit-field
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx3)
       (fxreverse-bit-field ?fx1 ?fx2 ?fx3))))

  (method-syntax rotate-bit-field
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx3 ?fx4)
       (fxrotate-bit-field ?fx1 ?fx2 ?fx3 ?fx4))))

  ;; methods: min and max
  (method-syntax max
    (syntax-rules ()
      ((_ ?fx1 ?fx ...)
       (fxmax ?fx1 ?fx ...))))

  (method-syntax min
    (syntax-rules ()
      ((_ ?fx1 ?fx ...)
       (fxmin ?fx1 ?fx ...)))))

;;; --------------------------------------------------------------------

(define-builtin-label <nonzero-fixnum>
  (parent <fixnum>)
  (predicate (lambda (fx)
	       (not ($fxzero? fx)))))

(define-builtin-label <nonpositive-fixnum>
  (parent <fixnum>)
  (predicate (lambda (fx)
	       ($fxnonpositive? fx))))

(define-builtin-label <nonnegative-fixnum>
  (parent <fixnum>)
  (predicate (lambda (fx)
	       ($fxnonnegative? fx))))

(define-builtin-label <positive-fixnum>
  (parent <nonnegative-fixnum>)
  (predicate (lambda (fx)
	       ($fxpositive? fx))))

(define-builtin-label <negative-fixnum>
  (parent <nonpositive-fixnum>)
  (predicate (lambda (fx)
	       ($fxnegative? fx))))

;;; --------------------------------------------------------------------

(define-builtin-label <flonum>
  (parent <real>)
  (predicate flonum?)

  (virtual-fields
   (immutable (integer?		<boolean>)	flinteger?)
   (immutable (finite?		<boolean>)	flfinite?)
   (immutable (infinite?	<boolean>)	flinfinite?)
   (immutable (nan?		<boolean>)	flnan?)
   (immutable (negative?	<boolean>)	flnegative?)
   (immutable (positive?	<boolean>)	flpositive?)
   (immutable (nonnegative?	<boolean>)	flnonnegative?)
   (immutable (nonpositive?	<boolean>)	flnonpositive?)
   (immutable (zero?		<boolean>)	flzero?)
   (immutable (zero?/positive	<boolean>)	flzero?/positive)
   (immutable (zero?/negative	<boolean>)	flzero?/negative)
   (immutable (even?		<boolean>)	fleven?)
   (immutable (odd?		<boolean>)	flodd?))

  ;; methods: conversion
  (method-syntax string
    (syntax-rules ()
      ((_ ?fx)
       (flonum->string ?fx))))

  ;; methods: arithmetic functions
  (method-syntax abs
    (syntax-rules ()
      ((_ ?fl)
       (flabs ?fl))))

  (method-syntax *
    (syntax-rules ()
      ((_ ?fl1 ?fl ...)
       (fl* ?fl1 ?fl ...))))

  (method-syntax +
    (syntax-rules ()
      ((_ ?fl1 ?fl ...)
       (fl+ ?fl1 ?fl ...))))

  (method-syntax -
    (syntax-rules ()
      ((_ ?fl1 ?fl ...)
       (fl- ?fl1 ?fl ...))))

  (method-syntax /
    (syntax-rules ()
      ((_ ?fl1 ?fl ...)
       (fl/ ?fl1 ?fl ...))))

  (method-syntax div
    (syntax-rules ()
      ((_ ?fl1 ?fl2)
       (fldiv ?fl1 ?fl2))))

  (method-syntax mod
    (syntax-rules ()
      ((_ ?fl1 ?fl2)
       (flmod ?fl1 ?fl2))))

  (method-syntax div-and-mod
    (syntax-rules ()
      ((_ ?fl1 ?fl2)
       (fldiv-and-mod ?fl1 ?fl2))))

  (method-syntax div0
    (syntax-rules ()
      ((_ ?fl1 ?fl2)
       (fldiv0 ?fl1 ?fl2))))

  (method-syntax mod0
    (syntax-rules ()
      ((_ ?fl1 ?fl2)
       (flmod0 ?fl1 ?fl2))))

  (method-syntax div0-and-mod0
    (syntax-rules ()
      ((_ ?fl1 ?fl2)
       (fldiv0-and-mod0 ?fl1 ?fl2))))

  ;; methods: power functions
  (method-syntax expt
    (syntax-rules ()
      ((_ ?fl1 ?fl2)
       (flexpt ?fl1 ?fl2))))

  (method-syntax square
    (syntax-rules ()
      ((_ ?fl)
       (flsquare ?fl))))

  (method-syntax cube
    (syntax-rules ()
      ((_ ?fl)
       (flcube ?fl))))

  (method-syntax sqrt
    (syntax-rules ()
      ((_ ?fl)
       (flsqrt ?fl))))

  (method-syntax cbrt
    (syntax-rules ()
      ((_ ?fl)
       (flcbrt ?fl))))

  ;; methods: comparison functions
  (method-syntax =
    (syntax-rules ()
      ((_ ?fl1 ?fl2 ?fl ...)
       (fl=? ?fl1 ?fl2 ?fl ...))))

  (method-syntax <
    (syntax-rules ()
      ((_ ?fl1 ?fl2 ?fl ...)
       (fl<? ?fl1 ?fl2 ?fl ...))))

  (method-syntax >
    (syntax-rules ()
      ((_ ?fl1 ?fl2 ?fl ...)
       (fl>? ?fl1 ?fl2 ?fl ...))))

  (method-syntax <=
    (syntax-rules ()
      ((_ ?fl1 ?fl2 ?fl ...)
       (fl<=? ?fl1 ?fl2 ?fl ...))))

  (method-syntax >=
    (syntax-rules ()
      ((_ ?fl1 ?fl2 ?fl ...)
       (fl>=? ?fl1 ?fl2 ?fl ...))))

  ;; methods: trigonometric functions
  (method-syntax sin
    (syntax-rules ()
      ((_ ?fl)
       (flsin ?fl))))

  (method-syntax cos
    (syntax-rules ()
      ((_ ?fl)
       (flcos ?fl))))

  (method-syntax tan
    (syntax-rules ()
      ((_ ?fl)
       (fltan ?fl))))

  (method-syntax acos
    (syntax-rules ()
      ((_ ?fl)
       (flacos ?fl))))

  (method-syntax asin
    (syntax-rules ()
      ((_ ?fl)
       (flasin ?fl))))

  (method-syntax atan
    (syntax-rules ()
      ((_ ?fl)
       (flatan ?fl))
      ((_ ?fl1 ?fl2)
       (flatan ?fl1 ?fl2))
      ))

  ;; methods: hyperbolic functions
  (method-syntax sinh
    (syntax-rules ()
      ((_ ?fl)
       (flsinh ?fl))))

  (method-syntax cosh
    (syntax-rules ()
      ((_ ?fl)
       (flcosh ?fl))))

  (method-syntax tanh
    (syntax-rules ()
      ((_ ?fl)
       (fltanh ?fl))))

  (method-syntax acosh
    (syntax-rules ()
      ((_ ?fl)
       (flacosh ?fl))))

  (method-syntax asinh
    (syntax-rules ()
      ((_ ?fl)
       (flasinh ?fl))))

  (method-syntax atanh
    (syntax-rules ()
      ((_ ?fl)
       (flatanh ?fl))))

  ;; methods: rounding functions
  (method-syntax ceiling
    (syntax-rules ()
      ((_ ?fl)
       (flceiling ?fl))))

  (method-syntax floor
    (syntax-rules ()
      ((_ ?fl)
       (flfloor ?fl))))

  (method-syntax round
    (syntax-rules ()
      ((_ ?fl)
       (flround ?fl))))

  (method-syntax truncate
    (syntax-rules ()
      ((_ ?fl)
       (fltruncate ?fl))))

  ;; methods: rationals operations
  (method-syntax numerator
    (syntax-rules ()
      ((_ ?fl)
       (flnumerator ?fl))))

  (method-syntax denominator
    (syntax-rules ()
      ((_ ?fl)
       (fldenominator ?fl))))

  ;; methods: exponentiation and logarithms
  (method-syntax exp
    (syntax-rules ()
      ((_ ?fl)
       (flexp ?fl))))

  (method-syntax log
    (syntax-rules ()
      ((_ ?fl)
       (fllog ?fl))
      ((_ ?fl1 ?fl2)
       (fllog ?fl1 ?fl2))
      ))

  (method-syntax log1p
    (syntax-rules ()
      ((_ ?fl)
       (fllog1p ?fl))))

  (method-syntax expm1
    (syntax-rules ()
      ((_ ?fl)
       (flexpm1 ?fl))))

  (method-syntax hypot
    (syntax-rules ()
      ((_ ?fl1 ?fl2)
       (flhypot ?fl1 ?fl2))))

  ;; methods: min and max
  (method-syntax max
    (syntax-rules ()
      ((_ ?fl1 ?fl ...)
       (flmax ?fl1 ?fl ...))))

  (method-syntax min
    (syntax-rules ()
      ((_ ?fl1 ?fl ...)
       (flmin ?fl1 ?fl ...))))

  #| end of label |# )


;;;; built-in types: procedure objects

(define-builtin-label <procedure>
  (predicate procedure?))


;;;; built-in types: special values

(define-label <unspecified>
  (predicate unspecified?)
  (protocol (lambda () (lambda () unspecified))))

(define-label <undefined>
  (predicate undefined?)
  (protocol (lambda () (lambda () undefined))))


(define (tag-unique-identifiers-of obj)
  ;;Return the list of UIDs in the class hierarchy of OBJ.  The order of
  ;;the tests is important.  More specialised types must come first.
  ;;
  (cond

   ;;In a previous version the test was (record? obj) but it would catch
   ;;all the  records, including the  ones not defined  by DEFINE-CLASS;
   ;;this  was  wrong.   Testing  for  <TOP>  (the  actual  record  type
   ;;predicate, not the "(<top>)" predicate) is slower but correct.
   ;;
   ((<top>? obj)			(<top>-unique-identifiers obj))

   ((number? obj)
    ;;Order does matter here!!!
    (cond ((fixnum?		obj)
	   ;;We  do  not  test   for  either  "<nonnegative-fixnum>"  or
	   ;;"<nonpositive-fixnum>".
	   (cond (($fxpositive? obj)	(tag-unique-identifiers <positive-fixnum>))
		 (($fxnegative? obj)	(tag-unique-identifiers <negative-fixnum>))
		 (else			(tag-unique-identifiers <fixnum>))))
	  ((bignum?		obj)	(tag-unique-identifiers <exact-integer>))
	  ((integer?		obj)	(tag-unique-identifiers <integer>))
	  ((rational?		obj)	(tag-unique-identifiers <rational>))
	  ((integer-valued?	obj)	(tag-unique-identifiers <integer-valued>))
	  ((rational-valued?	obj)	(tag-unique-identifiers <rational-valued>))
	  ((flonum?		obj)	(tag-unique-identifiers <flonum>))
	  ((real?		obj)	(tag-unique-identifiers <real>))
	  ((real-valued?	obj)	(tag-unique-identifiers <real-valued>))
	  ((complex?		obj)	(tag-unique-identifiers <complex>))
	  (else				(tag-unique-identifiers <number>))))

   ((char?		obj)		(tag-unique-identifiers <char>))
   ((string?		obj)		(tag-unique-identifiers <string>))
   ((symbol?		obj)		(tag-unique-identifiers <symbol>))
   ((vector?		obj)		(tag-unique-identifiers <vector>))
   ((bytevector?	obj)		(tag-unique-identifiers <bytevector>))

   ;;Notice that  we never try to  test for "<list>": qualifying  a long
   ;;list can be time-consuming.
   ((pair?		obj)		(if (pair? ($cdr obj))
					    (tag-unique-identifiers <spine>)
					  (tag-unique-identifiers <pair>)))
   ((null?		obj)		(tag-unique-identifiers <spine>))

   ((port?		obj)
    ;;Order here is arbitrary.
    (cond ((input/output-port?	obj)	(if (binary-port? obj)
					    (tag-unique-identifiers <binary-input/output-port>)
					  (tag-unique-identifiers <textual-input/output-port>)))
	  ((input-port?		obj)	(if (binary-port? obj)
					    (tag-unique-identifiers <binary-input-port>)
					  (tag-unique-identifiers <textual-input-port>)))
	  (else				#;(assert (output-port? obj))
					(if (binary-port? obj)
					    (tag-unique-identifiers <binary-output-port>)
					  (tag-unique-identifiers <textual-output-port>)))))
   ((transcoder?	obj)	(tag-unique-identifiers <transcoder>))
   ;;Remember that conditions are records by R6RS definition.
   ((condition?		obj)	(tag-unique-identifiers <condition>))
   ((record?		obj)	(tag-unique-identifiers <record>))
   ;;Remember that in Larceny hashtables are records.
   ((hashtable?	obj)	(tag-unique-identifiers <hashtable>))
   ((procedure? obj)	(tag-unique-identifiers <procedure>))

   ;;Everything else inluding records not defined by DEFINE-CLASS.
   (else (tag-unique-identifiers <top>))))


;;;; predefined multimethods and multimethods definers

(define-generic-definer  define-generic
  (argument-type-inspector	tag-unique-identifiers-of))

(define-generic*-definer define-generic*
  (argument-type-inspector	tag-unique-identifiers-of)
  (reverse-before-methods?	#f))

(define-generic object->string (o))

(define-method (object->string o)
  (call-with-string-output-port
      (lambda (port)
	(display o port))))


;;;; predefined multimethods: I/O ports

(module (put-single
	 put-multi
	 put-multi-2
	 put-multi-3
	 put-multi-4)
  (define-generic put-single  (port item))
  (define-generic put-multi-2 (port item))
  (define-generic put-multi-3 (port item start))
  (define-generic put-multi-4 (port item start count))

  (define-syntax put-multi
    (syntax-rules ()
      ((?port ?item)
       (put-multi-2 ?port ?item))
      ((?port ?item ?start)
       (put-multi-3 ?port ?item ?start))
      ((?port ?item start ?count)
       (put-multi-4 ?port ?item ?start ?count))))

  (add-method put-single (<binary-output-port>  <fixnum>) put-u8)
  (add-method put-single (<textual-output-port> <char>)   put-char)

  (add-method put-multi-2 (<binary-output-port> <bytevector>)                   put-bytevector)
  (add-method put-multi-3 (<binary-output-port> <bytevector> <fixnum>)          put-bytevector)
  (add-method put-multi-4 (<binary-output-port> <bytevector> <fixnum> <fixnum>) put-bytevector)

  (add-method put-multi-2 (<textual-output-port> <bytevector>)                   put-string)
  (add-method put-multi-3 (<textual-output-port> <bytevector> <fixnum>)          put-string)
  (add-method put-multi-4 (<textual-output-port> <bytevector> <fixnum> <fixnum>) put-string)

  #| end of module |# )

(module (get-single
	 lookahead-single
	 get-multi-n
	 get-multi-n!
	 get-multi-some
	 get-multi-all)

  (define-generic get-single (port))
  (define-generic lookahead-single (port))
  (define-generic get-multi-n (port count))
  (define-generic get-multi-n! (port item start count))
  (define-generic get-multi-some (port))
  (define-generic get-multi-all (port))

  (add-method get-single	(<binary-input-port>)	get-u8)
  (add-method get-single	(<textual-input-port>)	get-char)

  (add-method lookahead-single	(<binary-input-port>)	lookahead-u8)
  (add-method lookahead-single	(<textual-input-port>)	lookahead-char)

  (add-method get-multi-n	(<binary-input-port>  <fixnum>)	get-bytevector-n)
  (add-method get-multi-n	(<textual-input-port> <fixnum>)	get-string-n)

  (add-method get-multi-n!	(<binary-input-port>  <bytevector> <fixnum> <fixnum>)	get-bytevector-n!)
  (add-method get-multi-n!	(<textual-input-port> <string>     <fixnum> <fixnum>)	get-string-n!)

  (add-method get-multi-some	(<binary-input-port>)	get-bytevector-some)
  (add-method get-multi-some	(<textual-input-port>)	get-string-some)

  (add-method get-multi-all	(<binary-input-port>)	get-bytevector-all)
  (add-method get-multi-all	(<textual-input-port>)	get-string-all)

  #| end of module |# )


;;;; common mixins

(define-mixin <hashable-and-properties-clauses>
  (fields (mutable (%uid	<symbol>)))

  (virtual-fields
   (immutable (uid <symbol>)
	      (lambda/tags ((O <class>))
		(or (O $%uid)
		    (receive-and-return (sym)
			(gensym)
		      (set!/tags (O $%uid) sym)))))
   (immutable (hash <exact-integer>)
	      (lambda/tags ((O <class>))
		;;We memoize the hash value  in the "value" field of the
		;;symbol's data structure.
		(if ($unbound-object? ($symbol-value (O uid)))
		    (receive-and-return (H)
			(symbol-hash (O $%uid))
		      ($set-symbol-value! (O $%uid) H))
		  ($symbol-value (O $%uid))))))

  (method (putprop (O <class>) (key <symbol>) value)
    (putprop (O uid) key value))

  (method (getprop (O <class>) (key <symbol>))
    (getprop (O uid) key))

  (method (remprop (O <class>) (key <symbol>))
    (remprop (O uid) key))

  (method (property-list (O <class>))
    (property-list (O uid)))

  #| end of mixin |# )


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'aux.method-syntax 'scheme-indent-function 1)
;; eval: (put 'aux.method 'scheme-indent-function 1)
;; End:
