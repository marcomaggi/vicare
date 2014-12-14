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
;;;Copyright (C) 2013, 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(library (nausicaa language builtins (0 4))
  (options visit-upon-loading)
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
    <positive-bignum> <negative-bignum> <bignum>
    <exact-integer> <integer> <integer-valued>
    <ratnum> <rational> <rational-valued>
    <integer-flonum> <rational-flonum> <flonum>
    <real> <real-valued> <cflonum> <compnum> <complex> <number>
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
  (import (vicare (0 4))
    (only (vicare system $symbols)
	  $symbol-value
	  $symbol->string
	  $string->symbol
	  $set-symbol-value!
	  $unbound-object?
	  $putprop
	  $getprop
	  $remprop
	  $property-list)
    (vicare system $keywords)
    (vicare unsafe operations)
    (vicare language-extensions sentinels)
    (vicare containers bytevectors)
    (nausicaa language oopp (0 4))
    (nausicaa language multimethods (0 4)))


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
  (protocol
   (lambda ()
     (lambda/tags ({_ <symbol>} {S <string>})
       ($string->symbol S))))
  (predicate symbol?)
  (virtual-fields
   (immutable (brace string	<string>) symbol->string)
   (immutable (brace $string	<string>) $symbol->string)
   (immutable (brace hash	<fixnum>) symbol-hash)
   (immutable (brace $hash	<fixnum>) $symbol-hash)

   (immutable (brace bound?	<boolean>)	symbol-bound?)
   (immutable (brace $bound?	<boolean>)	$symbol-bound?)

   (mutable   (brace value	<top>)		symbol-value set-symbol-value!)
   (mutable   (brace $value	<top>)		$symbol-value $set-symbol-value!)
   #| end of virtual-fields |# )
  (method (putprop {S <symbol>} {K <symbol>} V)
    ($putprop S K V))
  (method (getprop {S <symbol>} {K <symbol>})
    ($getprop S K))
  (method (remprop {S <symbol>} {K <symbol>})
    ($remprop S K))
  (method (property-list {S <symbol>})
    ($property-list S))
  #| end of define-label |# )

(define-builtin-label <keyword>
  (predicate keyword?)
  (protocol
   (lambda ()
     (lambda/tags ({_ <keyword>} arg)
       (cond ((symbol? arg)
	      ($symbol->keyword arg))
	     ((string? arg)
	      ($symbol->keyword ($string->symbol arg)))
	     (else
	      (assertion-violation '<keyword>
		"expected symbol or string as constructor argument" arg))))))
  (virtual-fields
   (immutable (brace symbol	<symbol>)	keyword->symbol)
   (immutable (brace $symbol	<symbol>)	$keyword->symbol)
   (immutable (brace string	<string>)
	      (lambda/tags ({K <keyword>})
		(string-append "#:" ((K symbol) string))))
   #| end of virtual-fields |# )
  #| end of define-label |# )

(define-builtin-label <pointer>
  (predicate pointer?)
  (protocol (lambda () integer->pointer))
  (virtual-fields
   (immutable (brace null?	<boolean>)	pointer-null?)
   (immutable (brace integer	<integer>)	pointer->integer))

  (method-syntax (brace = <boolean>)
    (syntax-rules ()
      ((_ ?ptr1 ?ptr2)
       (pointer=? ?ptr1 ?ptr2))))

  (method-syntax (brace != <boolean>)
    (syntax-rules ()
      ((_ ?ptr1 ?ptr2)
       (pointer!=? ?ptr1 ?ptr2))))

  (method-syntax (brace < <boolean>)
    (syntax-rules ()
      ((_ ?ptr1 ?ptr2)
       (pointer<? ?ptr1 ?ptr2))))

  (method-syntax (brace > <boolean>)
    (syntax-rules ()
      ((_ ?ptr1 ?ptr2)
       (pointer>? ?ptr1 ?ptr2))))

  (method-syntax (brace <= <boolean>)
    (syntax-rules ()
      ((_ ?ptr1 ?ptr2)
       (pointer<=? ?ptr1 ?ptr2))))

  (method-syntax (brace >= <boolean>)
    (syntax-rules ()
      ((_ ?ptr1 ?ptr2)
       (pointer>=? ?ptr1 ?ptr2))))

  (method-syntax (brace add <pointer>)
    (syntax-rules ()
      ((_ ?ptr ?offfset)
       (pointer-add ?ptr ?offset))))

  (method-syntax (brace diff <exact-integer>)
    (syntax-rules ()
      ((_ ?ptr1 ?ptr2)
       (pointer-diff ?ptr1 ?ptr2))))

  (method-syntax (brace clone <pointer>)
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
	      (lambda/tags (A (brace D <spine>))
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

  (virtual-fields
   (immutable (brace length	<exact-integer>)	length)
   (immutable (brace null?	<boolean>) 		null?)
   (immutable (brace pair?	<boolean>)		pair?)
   (immutable car					car)
   (immutable (brace cdr	<spine>)		cdr)
   (immutable $car					$car)
   (immutable (brace $cdr	<spine>)		$cdr))

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

  (method-syntax (brace map <list>)
    (syntax-rules ()
      ((_ o proc . lists)
       (map proc o . lists))))

  (method-syntax for-each
    (syntax-rules ()
      ((_ o proc . lists)
       (for-each proc o . lists))))

  (method-syntax (brace reverse <list>)
    (syntax-rules ()
      ((_ o)
       (reverse o))))

  (method-syntax (brace append <list>)
    (syntax-rules ()
      ((_ o . lists)
       (append o . lists))))

;;; --------------------------------------------------------------------

  (method-syntax find
    (syntax-rules ()
      ((_ o proc)
       (find proc o))))

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

  (method-syntax (brace remp <list>)
    (syntax-rules ()
      ((_ o proc)
       (remp proc o))))

  (method-syntax (brace remove <list>)
    (syntax-rules ()
      ((_ o proc)
       (remove proc o))))

  (method-syntax (brace remv <list>)
    (syntax-rules ()
      ((_ o proc)
       (remv proc o))))

  (method-syntax (brace remq <list>)
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
  (virtual-fields
   (immutable car car)
   (immutable cdr cdr)
   (immutable $car $car)
   (immutable $cdr $cdr)))

(define-builtin-label <mutable-pair>
  (parent <pair>)
  (protocol (lambda () cons))
  (predicate pair?)
  (virtual-fields
   (mutable car car set-car!)
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
  (protocol (lambda ()
	      (lambda (item . items)
		(cons item items))))
  (predicate pair?))


;;;; built-in types: arrays common stuff

(define-mixin <array>
  (virtual-fields
   (immutable (brace empty?    <boolean>)
	      (lambda/tags ((brace S <array>))
		($fxzero? (S length))))
   (immutable (brace $empty?   <boolean>)
	      (lambda/tags ((brace S <array>))
		($fxzero? (S $length)))))

  (method (%normalise-index (brace S <array>) (brace idx <fixnum>))
    ;;This is private.  Non-negative indexes are handled as specified by
    ;;R6RS; negative  indexes are handled as  counts from the end  o the
    ;;array.
    ;;
    (define-fluid-override __who__ (identifier-syntax '%normalise-index))
    (cond ((or ($fxzero? idx)
	       (and ($fxpositive? idx)
		    ($fx< idx (S $length))))
	   idx)
	  ((and ($fxnegative? idx)
		($fx< ($fx- idx) (S $length)))
	   ($fx+ idx (S $length)))
	  (else
	   (assertion-violation __who__ "array index out of range" idx))))

  (getter (lambda (stx the-tag)
	    (syntax-case stx ()
	      ((?expr ((?idx)))
	       #`(#,the-tag #:oopp-syntax (?expr ref ?idx))))))

  #| end of mixin |# )

(define-mixin <mutable-array>
  (setter (lambda (stx the-tag)
	    (syntax-case stx ()
	      ((?expr ((?index)) ?val)
	       #`(#,the-tag #:oopp-syntax (?expr set! ?index ?val)))
	      )))
  #| end of mixin |# )


;;;; built-in types: characters and strings

(define-builtin-label <char>
  (predicate char?)
  (virtual-fields
   (immutable (brace upcase    <char>)	char-upcase)
   (immutable (brace downcase  <char>)	char-downcase)
   (immutable (brace titlecase <char>)	char-titlecase)
   (immutable (brace foldcase  <char>)	char-foldcase)

   (immutable (brace alphabetic? <boolean>)	char-alphabetic?)
   (immutable (brace numeric?    <boolean>)	char-numeric?)
   (immutable (brace whitespace? <boolean>)	char-whitespace?)
   (immutable (brace upper-case? <boolean>)	char-upper-case?)
   (immutable (brace lower-case? <boolean>)	char-lower-case?)
   (immutable (brace title-case? <boolean>)	char-title-case?)

   (immutable (brace general-category <symbol>) char-general-category)))

;;; --------------------------------------------------------------------

(define-builtin-label <string>
  (mixins <array> <mutable-array>)
  (protocol (lambda () string))
  (predicate string?)
  (virtual-fields
   (immutable (brace length		<fixnum>)			string-length)
   (immutable (brace $length		<fixnum>)			$string-length)
   (immutable (brace upcase		<string>)			string-upcase)
   (immutable (brace downcase		<string>)			string-downcase)
   (immutable (brace titlecase		<string>)			string-titlecase)
   (immutable (brace foldcase		<string>)			string-foldcase)

   (immutable (brace ascii		<ascii-bytevector>)		string->ascii)
   (immutable (brace latin1		<bytevector-u8>)		string->latin1)
   (immutable (brace utf8		<bytevector-u8>)		string->utf8)
   (immutable (brace utf16		<bytevector>)			string->utf16)
   (immutable (brace utf16le		<bytevector-u16l>)		string->utf16le)
   (immutable (brace utf16be		<bytevector-u16b>)		string->utf16be)
   (immutable (brace utf16n		<bytevector-u16n>)		string->utf16n)
   (immutable (brace utf32		<bytevector-u32>)		string->utf32)
   (immutable (brace percent-encoding	<percent-encoded-bytevector>)	string->uri-encoding)

   (immutable (brace $ascii		<ascii-bytevector>)		$string->ascii)
   (immutable (brace $latin1		<bytevector-u8>)		$string->latin1)

   #| end of virtual-fields |#)

  (method (ref (brace S <string>) (brace idx <fixnum>))
    ($string-ref S (S %normalise-index idx)))

  (method-syntax (brace hash <fixnum>)
    (syntax-rules ()
      ((_ ?o)
       (string-hash ?o))))

  (method (brace substring <string>)
    (case-lambda/tags
      (((brace S <string>) (brace start <fixnum>))
       ($substring S
		   (S %normalise-index start)
		   (S $length)))
      (((brace S <string>) (brace start <fixnum>) (brace end <fixnum>))
       ($substring S
		   (S %normalise-index start)
		   (S %normalise-index end)))))

  (method-syntax (brace append <string>)
    (syntax-rules ()
      ((_ ?o . ?strings)
       (string-append ?o . ?strings))))

  (method-syntax (brace list <list>)
    (syntax-rules ()
      ((_ ?o)
       (string->list ?o))))

  (method-syntax for-each
    (syntax-rules ()
      ((_ ?o ?proc)
       (string-for-each ?proc ?o))))

  (method-syntax (brace copy <string>)
    (syntax-rules ()
      ((_ ?o)
       (string-copy ?o)))))

(define-builtin-label <mutable-string>
  (parent <string>)
  (mixins <mutable-array>)
  (protocol (lambda () string))
  (predicate string?)
  (method (set! (brace S <mutable-string>) (brace idx <fixnum>) (brace ch <char>))
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
  (virtual-fields
   (immutable (brace length	<fixnum>)	vector-length)
   (immutable (brace $length	<fixnum>)	$vector-length))

;;; --------------------------------------------------------------------

  (method (ref (brace S <vector>) (brace idx <fixnum>))
    ($vector-ref S (S %normalise-index idx)))

  (method (set! (brace S <vector>) (brace idx <fixnum>) item)
    ($vector-set! S (S %normalise-index idx) item))

;;; --------------------------------------------------------------------

  (method (brace subvector <vector>)
    (case-lambda/tags
      (((brace S <vector>) (brace start <fixnum>))
       ($subvector S
		   (S %normalise-index start)
		   (S $length)))
      (((brace S <vector>) (brace start <fixnum>) (brace end <fixnum>))
       ($subvector S
		   (S %normalise-index start)
		   (S %normalise-index end)))))

  (method-syntax (brace map <vector>)
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
   (immutable (brace length		<fixnum>)			bytevector-length)
   (immutable (brace $length		<fixnum>)			$bytevector-length)

   (immutable (brace percent-encoded?	<boolean>)			percent-encoded-bytevector?)
   (immutable (brace $percent-encoded?	<boolean>)			$percent-encoded-bytevector?)

   (immutable (brace ascii-encoded?	<boolean>)			ascii-encoded-bytevector?)
   (immutable (brace $ascii-encoded?	<boolean>)			$ascii-encoded-bytevector?)

   (immutable (brace latin1-encoded?	<boolean>)			latin1-encoded-bytevector?)
   (immutable (brace $latin1-encoded?	<boolean>)			$latin1-encoded-bytevector?)

   (immutable (brace percent-encoded	<percent-encoded-bytevector>)	percent-encode)
   (immutable (brace $percent-encoded	<percent-encoded-bytevector>)	$percent-encode)

   (immutable (brace percent-decoded	<bytevector>)			percent-decode)
   (immutable (brace $percent-decoded	<bytevector>)			$percent-decode)

   (immutable (brace octets-string	<string>)			octets->string)
   (immutable (brace ascii-string	<string>)			ascii->string)
   (immutable (brace latin1-string	<string>)			latin1->string)
   (immutable (brace uri-string		<string>)			uri-encoding->string)
   (immutable (brace utf8-string	<string>)			utf8->string)
   (immutable (brace utf16be-string	<string>)			utf16be->string)
   (immutable (brace utf16le-string	<string>)			utf16le->string)
   (immutable (brace utf16n-string	<string>)			utf16n->string)

   #| end of virtual-fields|# )

  (method-syntax (brace copy <bytevector>)
    (syntax-rules ()
      ((_ ?bv)
       (bytevector-copy ?bv))))

  (method-syntax (brace $copy <bytevector>)
    (syntax-rules ()
      ((_ ?bv)
       ($bytevector-copy ?bv))))

  (method-syntax (brace hash <fixnum>)
    (syntax-rules ()
      ((_ ?bv)
       (bytevector-hash ?bv))))

  (method-syntax (brace $hash <fixnum>)
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
		    (setter (lambda (stx the-tag)
			      (syntax-case stx ()
				((?expr ((?idx)) ?val)
				 #'(SETTER ?expr ?idx ?val)))))
		    (getter (lambda (stx the-tag)
			      (syntax-case stx ()
				((?expr ((?idx)))
				 #'(GETTER ?expr ?idx))))))
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
  (virtual-fields
   (immutable (brace size <exact-integer>)	hashtable-size)
   (immutable (brace keys <vector>)		hashtable-keys)
   (immutable (brace mutable? <boolean>)	hashtable-mutable?))
  (getter (lambda (stx the-tag)
	    (syntax-case stx ()
	      ((?expr ((?key)))
	       #'(hashtable-ref ?expr ?key (void)))
	      ((?expr ((?key) (?default)))
	       #'(hashtable-ref ?expr ?key ?default))
	      )))

  (setter (lambda (stx the-tag)
	    (syntax-case stx ()
	      ((?expr ((?index)) ?val)
	       #'(hashtable-set! ?expr ?index ?val)))))

  ;;This returns 2 values, so it cannot be a field.
  (method-syntax entries
    (syntax-rules ()
      ((_ ?table)
       (hashtable-entries ?table))))

  (method-syntax delete!
    (syntax-rules ()
      ((_ ?table ?key)
       (hashtable-delete! ?table ?key))))

  (method-syntax (brace contains? <boolean>)
    (syntax-rules ()
      ((_ ?table ?key)
       (hashtable-contains? ?table ?key))))

  (method-syntax clear!
    (syntax-rules ()
      ((_ ?table)
       (hashtable-clear! ?table))))

  (method-syntax (brace copy <hashtable>)
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
  (virtual-fields
   (immutable (brace name		<symbol>)			record-type-name)
   (immutable (brace parent		<record-type-descriptor>)	record-type-parent)
   (immutable (brace uid		<symbol>)			record-type-uid)
   (immutable (brace generative?	<boolean>)			record-type-generative?)
   (immutable (brace sealed?		<boolean>)			record-type-sealed?)
   (immutable (brace opaque?		<boolean>)			record-type-opaque?)
   (immutable (brace field-names	<vector>)			record-type-field-names))

  (method-syntax (brace predicate <procedure>)
    (syntax-rules ()
      ((_ ?rtd)
       (record-predicate ?rtd))))

  (method-syntax (brace accessor <procedure>)
    (syntax-rules ()
      ((_ ?rtd ?field-idx)
       (record-accessor ?rtd ?field-idx))))

  (method-syntax (brace mutator <procedure>)
    (syntax-rules ()
      ((_ ?rtd ?field-idx)
       (record-mutator ?rtd ?field-idx))))

  (method-syntax (brace field-mutable? <boolean>)
    (syntax-rules ()
      ((_ ?rtd ?field-idx)
       (record-field-mutable? ?rtd ?field-idx))))

  #| end of label |# )

(define-builtin-label <record>
  (predicate record?)
  (virtual-fields
   (immutable (brace rtd <record-type-descriptor>) record-rtd)))


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
  (virtual-fields
   (immutable codec transcoder-codec)
   (immutable (brace eol-style <symbol>)		transcoder-eol-style)
   (immutable (brace error-handling-mode <symbol>)	transcoder-error-handling-mode)))


;;;; built-in types: port objects

(define-builtin-label <port>
  (predicate port?)
  (virtual-fields
   (immutable (brace transcoder			<transcoder>)	port-transcoder)

   (immutable (brace textual?			<boolean>)	textual-port?)
   (immutable (brace binary?			<boolean>)	binary-port?)
   (immutable (brace input?			<boolean>)	input-port?)
   (immutable (brace output?			<boolean>)	output-port?)

   (immutable (brace has-port-position?		<boolean>)	port-has-port-position?)
   (immutable (brace has-set-port-position?	<boolean>)	port-has-set-port-position!?)
   (mutable   (brace port-position		<integer>)	port-position set-port-position!)

   (immutable (brace closed?			<boolean>)	port-closed?)

   (immutable fd						port-fd)
   (immutable (brace id				<string>)	port-id)
   (immutable (brace uid			<symbol>)	port-uid)
   (immutable (brace hash			<fixnum>)	port-hash)

   (mutable   (brace non-blocking-mode?	<boolean>)
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
     (immutable (brace eof? <boolean>) port-eof?))
    (methods (get-single	get-single)
	     (lookahead-single	lookahead-single)
	     (get-multi-n	get-multi-n)
	     (get-multi-n!	get-multi-n!)
	     (get-multi-some	get-multi-some)
	     (get-multi-all	get-multi-all)))

  (define-mixin <output-port-clauses>
    (virtual-fields
     (mutable (brace buffer-mode <symbol>) output-port-buffer-mode	set-port-buffer-mode!))
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
   (immutable (brace zero?		<boolean>)	zero?)
   (immutable (brace finite?		<boolean>)	finite?)
   (immutable (brace infinite?		<boolean>)	infinite?)
   (immutable (brace nan?		<boolean>)	nan?))

  ;; exactness
  (virtual-fields
   (immutable (brace exact?		<boolean>)	exact?)
   (immutable (brace inexact?		<boolean>)	inexact?))

  ;; conversion
  (virtual-fields
   (immutable (brace string		<string>)	number->string))

  ;;This method supports the optional arguments of NUMBER->STRING.
  (methods ({string-radix <string>} number->string))

;;; math functions from (rnrs base (6)) and (vicare)

  ;; arithmetic
  (method-syntax (brace + <number>)
    (syntax-rules ()
      ((_ ?num . ?nums)
       (+ ?num . ?nums))))

  (method-syntax (brace - <number>)
    (syntax-rules ()
      ((_ ?num . ?nums)
       (- ?num . ?nums))))

  (method-syntax (brace * <number>)
    (syntax-rules ()
      ((_ ?num . ?nums)
       (* ?num . ?nums))))

  (method-syntax (brace / <number>)
    (syntax-rules ()
      ((_ ?num . ?nums)
       (/ ?num . ?nums))))

  ;; exactness
  (method-syntax (brace exact <number>)
    (syntax-rules ()
      ((_ ?num)
       (exact ?num))))

  (method-syntax (brace inexact <number>)
    (syntax-rules ()
      ((_ ?num)
       (inexact ?num))))

  ;; powers
  (method-syntax (brace expt <number>)
    (syntax-rules ()
      ((_ ?num ?exp)
       (expt ?num ?exp))))

  (method-syntax (brace square <number>)
    (syntax-rules ()
      ((_ ?num)
       (square ?num))))

  (method-syntax (brace cube <number>)
    (syntax-rules ()
      ((_ ?num)
       (cube ?num))))

  (method-syntax (brace sqrt <number>)
    (syntax-rules ()
      ((_ ?num)
       (sqrt ?num))))

  ;; exponentiation and logarithms
  (method-syntax (brace exp <number>)
    (syntax-rules ()
      ((_ ?num)
       (exp ?num))))

  (method-syntax (brace log <number>)
    (syntax-rules ()
      ((_ ?num)
       (log ?num))
      ((_ ?num ?base)
       (log ?num ?base))
      ))

  ;; trigonometric functions
  (method-syntax (brace sin <number>)
    (syntax-rules ()
      ((_ ?num)
       (sin ?num))))

  (method-syntax (brace cos <number>)
    (syntax-rules ()
      ((_ ?num)
       (cos ?num))))

  (method-syntax (brace tan <number>)
    (syntax-rules ()
      ((_ ?num)
       (tan ?num))))

  (method-syntax (brace asin <number>)
    (syntax-rules ()
      ((_ ?num)
       (asin ?num))))

  (method-syntax (brace acos <number>)
    (syntax-rules ()
      ((_ ?num)
       (acos ?num))))

  (method-syntax (brace atan <number>)
    (syntax-rules ()
      ((_ ?num)
       (atan ?num))))

  ;; hyperbolic functions
  (method-syntax (brace sinh <number>)
    (syntax-rules ()
      ((_ ?num)
       (sinh ?num))))

  (method-syntax (brace cosh <number>)
    (syntax-rules ()
      ((_ ?num)
       (cosh ?num))))

  (method-syntax (brace tanh <number>)
    (syntax-rules ()
      ((_ ?num)
       (tanh ?num))))

  (method-syntax (brace asinh <number>)
    (syntax-rules ()
      ((_ ?num)
       (asinh ?num))))

  (method-syntax (brace acosh <number>)
    (syntax-rules ()
      ((_ ?num)
       (acosh ?num))))

  (method-syntax (brace atanh <number>)
    (syntax-rules ()
      ((_ ?num)
       (atanh ?num))))

  ;; complex numbers typical operations
  (method-syntax (brace conjugate <number>)
    (syntax-rules ()
      ((_ ?num)
       (complex-conjugate ?num))))

  (method-syntax (brace real-part <real>)
    (syntax-rules ()
      ((_ ?num)
       (real-part ?num))))

  (method-syntax (brace imag-part <real>)
    (syntax-rules ()
      ((_ ?num)
       (imag-part ?num))))

  (method-syntax (brace magnitude <real>)
    (syntax-rules ()
      ((_ ?num)
       (magnitude ?num))))

  (method-syntax (brace angle <real>)
    (syntax-rules ()
      ((_ ?num)
       (angle ?num)))))

;;; --------------------------------------------------------------------

(define-builtin-label <complex>
  (parent <number>)
  (predicate complex?))

(define-builtin-label <cflonum>
  (parent <complex>)
  (predicate cflonum?))

(define-builtin-label <compnum>
  (parent <complex>)
  (predicate compnum?))

;;; --------------------------------------------------------------------

(define-builtin-label <real-valued>
  (parent <complex>)
  (predicate real-valued?))

;;; --------------------------------------------------------------------

(define-builtin-label <real>
  (parent <real-valued>)
  (predicate real?)
  (virtual-fields
   (immutable (brace abs		<real>)		abs)
   (immutable (brace positive?		<boolean>)	positive?)
   (immutable (brace negative?		<boolean>)	negative?)
   (immutable (brace non-positive?	<boolean>)	non-positive?)
   (immutable (brace non-negative?	<boolean>)	non-negative?)
   (immutable (brace sign		<fixnum>)	sign))
  ;; rational numbers typical operations
  (virtual-fields
   (immutable (brace numerator		<real>)		numerator)
   (immutable (brace denominator	<real>)		denominator))
  ;; methods: rounding
  (method-syntax {floor <real>}
    (syntax-rules ()
      ((_ ?num)
       (floor ?num))))

  (method-syntax {ceiling <real>}
    (syntax-rules ()
      ((_ ?num)
       (ceiling ?num))))

  (method-syntax {truncate <real>}
    (syntax-rules ()
      ((_ ?num)
       (truncate ?num))))

  (method-syntax {round <real>}
    (syntax-rules ()
      ((_ ?num)
       (round ?num))))

  (method-syntax {rationalize <real>}
    (syntax-rules ()
      ((_ ?num ?tolerance)
       (rationalize ?num ?tolerance)))))

;;; --------------------------------------------------------------------

(define-builtin-label <rational-valued>
  (parent <real>)
  (predicate rational-valued?))

;;; --------------------------------------------------------------------

(define-builtin-label <rational>
  (parent <rational-valued>)
  (predicate rational?))

;;; --------------------------------------------------------------------

(define-builtin-label <ratnum>
  (parent <rational>)
  (predicate ratnum?)
  (virtual-fields
   (immutable (brace numerator		<exact-integer>)	numerator)
   (immutable (brace denominator	<exact-integer>)	denominator)))

;;; --------------------------------------------------------------------

(define-builtin-label <integer-valued>
  (parent <rational-valued>)
  (predicate integer-valued?))

;;; --------------------------------------------------------------------

(define-builtin-label <integer>
  (parent <integer-valued>)
  (predicate integer?)
  (virtual-fields
   (immutable (brace odd?	<boolean>)	odd?)
   (immutable (brace even?	<boolean>)	even?)))

;;; --------------------------------------------------------------------

(define-builtin-label <exact-integer>
  (parent <integer>)
  (predicate exact-integer?))

;;; --------------------------------------------------------------------

(define-builtin-label <bignum>
  (parent <exact-integer>)
  (predicate bignum?))

(define-builtin-label <positive-bignum>
  (parent <bignum>)
  (predicate $bignum-positive?))

(define-builtin-label <negative-bignum>
  (parent <bignum>)
  (predicate $bignum-negative?))

;;; --------------------------------------------------------------------

(define-builtin-label <fixnum>
  (parent <exact-integer>)
  (predicate fixnum?)

  (virtual-fields
   (immutable (brace even?		<boolean>)	fxeven?)
   (immutable (brace odd?		<boolean>)	fxodd?)
   (immutable (brace negative?		<boolean>)	fxnegative?)
   (immutable (brace positive?		<boolean>)	fxpositive?)
   (immutable (brace non-negative?	<boolean>)	fxnonnegative?)
   (immutable (brace non-positive?	<boolean>)	fxnonpositive?)
   (immutable (brace zero?		<boolean>)	fxzero?)
   (immutable (brace sign		<fixnum>)	fxsign)

   (immutable (brace $even?		<boolean>)	$fxeven?)
   (immutable (brace $odd?		<boolean>)	$fxodd?)
   (immutable (brace $negative?		<boolean>)	$fxnegative?)
   (immutable (brace $positive?		<boolean>)	$fxpositive?)
   (immutable (brace $non-negative?	<boolean>)	$fxnonnegative?)
   (immutable (brace $non-positive?	<boolean>)	$fxnonpositive?)
   (immutable (brace $zero?		<boolean>)	$fxzero?)
   (immutable (brace $sign		<fixnum>)	$fxsign)
   #| end of virtual-fields |# )

  ;; methods: conversion
  (method-syntax (brace string <string>)
    (syntax-rules ()
      ((_ ?fx)
       (fixnum->string ?fx))
      ((_ ?fx ?base)
       (fixnum->string ?fx ?base))
      ))

  (method-syntax (brace $string <string>)
    (syntax-rules ()
      ((_ ?fx)
       ($fixnum->string ?fx 10))
      ((_ ?fx ?base)
       ($fixnum->string ?fx ?base))
      ))

  (method-syntax (brace flonum <flonum>)
    (syntax-rules ()
      ((_ ?fx)
       (fixnum->flonum ?fx))))

  (method-syntax (brace $flonum <flonum>)
    (syntax-rules ()
      ((_ ?fx)
       ($fixnum->flonum ?fx))))

  ;; methods: arithmetic operations
  (method-syntax (brace abs <exact-integer>)
    (syntax-rules ()
      ((_ ?fx)
       (fxabs ?fx))))

  (method-syntax (brace * <exact-integer>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fx* ?fx1 ?fx2))))

  (method-syntax mul-with-carry
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx3)
       (fx*/carry ?fx1 ?fx2 ?fx3))))

  (method-syntax (brace + <exact-integer>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fx+ ?fx1 ?fx2))))

  (method-syntax add-with-carry
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx3)
       (fx+/carry ?fx1 ?fx2 ?fx3))))

  (method-syntax (brace add1 <exact-integer>)
    (syntax-rules ()
      ((_ ?fx)
       (fx+ ?fx 1))))

  (method-syntax (brace $add1 <exact-integer>)
    (syntax-rules ()
      ((_ ?fx)
       ($fxadd1 ?fx))))

  (method-syntax (brace - <exact-integer>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fx- ?fx1 ?fx2))))

  (method-syntax sub-with-carry
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx3)
       (fx-/carry ?fx1 ?fx2 ?fx3))))

  (method-syntax (brace sub1 <exact-integer>)
    (syntax-rules ()
      ((_ ?fx)
       (fx- ?fx 1))))

  (method-syntax (brace $sub1 <exact-integer>)
    (syntax-rules ()
      ((_ ?fx)
       ($fxsub1 ?fx))))

  (method-syntax (brace div <exact-integer>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fxdiv ?fx1 ?fx2))))

  (method-syntax (brace mod <exact-integer>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fxmod ?fx1 ?fx2))))

  (method-syntax div-and-mod
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fxdiv-and-mod ?fx1 ?fx2))))

  (method-syntax (brace div0 <exact-integer>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fxdiv0 ?fx1 ?fx2))))

  (method-syntax (brace mod0 <exact-integer>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fxmod0 ?fx1 ?fx2))))

  (method-syntax div0-and-mod0
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fxdiv0-and-mod0 ?fx1 ?fx2))))

  ;; methods: comparison operations
  (method-syntax (brace = <boolean>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx ...)
       (fx=? ?fx1 ?fx2 ?fx ...))))

  (method-syntax (brace < <boolean>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx ...)
       (fx<? ?fx1 ?fx2 ?fx ...))))

  (method-syntax (brace > <boolean>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx ...)
       (fx>? ?fx1 ?fx2 ?fx ...))))

  (method-syntax (brace <= <boolean>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx ...)
       (fx<=? ?fx1 ?fx2 ?fx ...))))

  (method-syntax (brace >= <boolean>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx ...)
       (fx>=? ?fx1 ?fx2 ?fx ...))))

  ;; methods: logic operations
  (method-syntax (brace and <fixnum>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx ...)
       (fxand ?fx1 ?fx2 ?fx ...))))

  (method-syntax (brace ior <fixnum>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx ...)
       (fxior ?fx1 ?fx2 ?fx ...))))

  (method-syntax (brace xor <fixnum>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx ...)
       (fxxor ?fx1 ?fx2 ?fx ...))))

  (method-syntax (brace not <fixnum>)
    (syntax-rules ()
      ((_ ?fx)
       (fxnot ?fx))))

  ;; methods: shift operations
  (method-syntax (brace arithmetic-shift <fixnum>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fxarithmetic-shift ?fx1 ?fx2))))

  (method-syntax (brace arithmetic-shift-left <fixnum>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fxarithmetic-shift-left ?fx1 ?fx2))))

  (method-syntax (brace arithmetic-shift-right <fixnum>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fxarithmetic-shift-right ?fx1 ?fx2))))

  ;; methods: bitwise operations
  (method-syntax (brace bit-set? <boolean>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2)
       (fxbit-set? ?fx1 ?fx2))))

  (method-syntax (brace bit-count <fixnum>)
    (syntax-rules ()
      ((_ ?fx)
       (fxbit-count ?fx))))

  (method-syntax (brace bit-field <fixnum>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx3)
       (fxbit-field ?fx1 ?fx2 ?fx3))))

  (method-syntax (brace copy-bit <fixnum>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx3)
       (fxcopy-bit ?fx1 ?fx2 ?fx3))))

  (method-syntax (brace copy-bit-field <fixnum>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx3 ?fx4)
       (fxcopy-bit-field ?fx1 ?fx2 ?fx3 ?fx4))))

  (method-syntax (brace first-bit-set <fixnum>)
    (syntax-rules ()
      ((_ ?fx)
       (fxfirst-bit-set ?fx))))

  (method-syntax (brace if <fixnum>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx3)
       (fxif ?fx1 ?fx2 ?fx3))))

  (method-syntax (brace length <fixnum>)
    (syntax-rules ()
      ((_ ?fx)
       (fxlength ?fx))))

  (method-syntax (brace reverse-bit-field <fixnum>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx3)
       (fxreverse-bit-field ?fx1 ?fx2 ?fx3))))

  (method-syntax (brace rotate-bit-field <fixnum>)
    (syntax-rules ()
      ((_ ?fx1 ?fx2 ?fx3 ?fx4)
       (fxrotate-bit-field ?fx1 ?fx2 ?fx3 ?fx4))))

  ;; methods: min and max
  (method-syntax (brace max <fixnum>)
    (syntax-rules ()
      ((_ ?fx1 ?fx ...)
       (fxmax ?fx1 ?fx ...))))

  (method-syntax (brace min <fixnum>)
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
   (immutable (brace integer?		<boolean>)	flinteger?)
   (immutable (brace finite?		<boolean>)	flfinite?)
   (immutable (brace infinite?		<boolean>)	flinfinite?)
   (immutable (brace nan?		<boolean>)	flnan?)
   (immutable (brace negative?		<boolean>)	flnegative?)
   (immutable (brace positive?		<boolean>)	flpositive?)
   (immutable (brace nonnegative?	<boolean>)	flnonnegative?)
   (immutable (brace nonpositive?	<boolean>)	flnonpositive?)
   (immutable (brace zero?		<boolean>)	flzero?)
   (immutable (brace zero?/positive	<boolean>)	flzero?/positive)
   (immutable (brace zero?/negative	<boolean>)	flzero?/negative)
   (immutable (brace even?		<boolean>)	fleven?)
   (immutable (brace odd?		<boolean>)	flodd?)

   (immutable (brace $integer?		<boolean>)	$flonum-integer?)
   (immutable (brace $finite?		<boolean>)	$flfinite?)
   (immutable (brace $infinite?		<boolean>)	$flinfinite?)
   (immutable (brace $nan?		<boolean>)	$flnan?)
   (immutable (brace $negative?		<boolean>)	$flnegative?)
   (immutable (brace $positive?		<boolean>)	$flpositive?)
   (immutable (brace $nonnegative?	<boolean>)	$flnonnegative?)
   (immutable (brace $nonpositive?	<boolean>)	$flnonpositive?)
   (immutable (brace $zero?		<boolean>)	$flzero?)
   (immutable (brace $zero?/positive	<boolean>)	$flzero?/positive)
   (immutable (brace $zero?/negative	<boolean>)	$flzero?/negative)
   (immutable (brace $even?		<boolean>)	$fleven?)
   (immutable (brace $odd?		<boolean>)	$flodd?)
   #| end of virtual-fields |# )

  ;; methods: conversion
  (method-syntax (brace string <string>)
    (syntax-rules ()
      ((_ ?fx)
       (flonum->string ?fx))))

  ;; methods: arithmetic functions
  (method-syntax (brace abs <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flabs ?fl))))

  (method-syntax (brace $abs <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       ($flabs ?fl))))

  (method-syntax (brace * <flonum>)
    (syntax-rules ()
      ((_ ?fl1 ?fl ...)
       (fl* ?fl1 ?fl ...))))

  (method-syntax (brace + <flonum>)
    (syntax-rules ()
      ((_ ?fl1 ?fl ...)
       (fl+ ?fl1 ?fl ...))))

  (method-syntax (brace - <flonum>)
    (syntax-rules ()
      ((_ ?fl1 ?fl ...)
       (fl- ?fl1 ?fl ...))))

  (method-syntax (brace / <flonum>)
    (syntax-rules ()
      ((_ ?fl1 ?fl ...)
       (fl/ ?fl1 ?fl ...))))

  (method-syntax (brace div <flonum>)
    (syntax-rules ()
      ((_ ?fl1 ?fl2)
       (fldiv ?fl1 ?fl2))))

  (method-syntax (brace mod <flonum>)
    (syntax-rules ()
      ((_ ?fl1 ?fl2)
       (flmod ?fl1 ?fl2))))

  (method-syntax div-and-mod
    (syntax-rules ()
      ((_ ?fl1 ?fl2)
       (fldiv-and-mod ?fl1 ?fl2))))

  (method-syntax (brace div0 <flonum>)
    (syntax-rules ()
      ((_ ?fl1 ?fl2)
       (fldiv0 ?fl1 ?fl2))))

  (method-syntax (brace mod0 <flonum>)
    (syntax-rules ()
      ((_ ?fl1 ?fl2)
       (flmod0 ?fl1 ?fl2))))

  (method-syntax div0-and-mod0
    (syntax-rules ()
      ((_ ?fl1 ?fl2)
       (fldiv0-and-mod0 ?fl1 ?fl2))))

  ;; methods: power functions
  (method-syntax (brace expt <flonum>)
    (syntax-rules ()
      ((_ ?fl1 ?fl2)
       (flexpt ?fl1 ?fl2))))

  (method-syntax (brace square <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flsquare ?fl))))

  (method-syntax (brace cube <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flcube ?fl))))

  (method-syntax (brace sqrt <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flsqrt ?fl))))

  (method-syntax (brace cbrt <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flcbrt ?fl))))

  ;; methods: comparison functions
  (method-syntax (brace = <boolean>)
    (syntax-rules ()
      ((_ ?fl1 ?fl2 ?fl ...)
       (fl=? ?fl1 ?fl2 ?fl ...))))

  (method-syntax (brace < <boolean>)
    (syntax-rules ()
      ((_ ?fl1 ?fl2 ?fl ...)
       (fl<? ?fl1 ?fl2 ?fl ...))))

  (method-syntax (brace > <boolean>)
    (syntax-rules ()
      ((_ ?fl1 ?fl2 ?fl ...)
       (fl>? ?fl1 ?fl2 ?fl ...))))

  (method-syntax (brace <= <boolean>)
    (syntax-rules ()
      ((_ ?fl1 ?fl2 ?fl ...)
       (fl<=? ?fl1 ?fl2 ?fl ...))))

  (method-syntax (brace >= <boolean>)
    (syntax-rules ()
      ((_ ?fl1 ?fl2 ?fl ...)
       (fl>=? ?fl1 ?fl2 ?fl ...))))

  ;; methods: trigonometric functions
  (method-syntax (brace sin <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flsin ?fl))))

  (method-syntax (brace cos <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flcos ?fl))))

  (method-syntax (brace tan <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (fltan ?fl))))

  (method-syntax (brace acos <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flacos ?fl))))

  (method-syntax (brace asin <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flasin ?fl))))

  (method-syntax (brace atan <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flatan ?fl))
      ((_ ?fl1 ?fl2)
       (flatan ?fl1 ?fl2))
      ))

  ;; methods: hyperbolic functions
  (method-syntax (brace sinh <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flsinh ?fl))))

  (method-syntax (brace cosh <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flcosh ?fl))))

  (method-syntax (brace tanh <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (fltanh ?fl))))

  (method-syntax (brace acosh <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flacosh ?fl))))

  (method-syntax (brace asinh <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flasinh ?fl))))

  (method-syntax (brace atanh <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flatanh ?fl))))

  ;; methods: rounding functions
  (method-syntax (brace ceiling <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flceiling ?fl))))

  (method-syntax (brace floor <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flfloor ?fl))))

  (method-syntax (brace round <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flround ?fl))))

  (method-syntax (brace truncate <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (fltruncate ?fl))))

  ;; methods: rationals operations
  (method-syntax (brace numerator <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flnumerator ?fl))))

  (method-syntax (brace denominator <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (fldenominator ?fl))))

  ;; methods: exponentiation and logarithms
  (method-syntax (brace exp <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flexp ?fl))))

  (method-syntax (brace log <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (fllog ?fl))
      ((_ ?fl1 ?fl2)
       (fllog ?fl1 ?fl2))
      ))

  (method-syntax (brace log1p <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (fllog1p ?fl))))

  (method-syntax (brace expm1 <flonum>)
    (syntax-rules ()
      ((_ ?fl)
       (flexpm1 ?fl))))

  (method-syntax (brace hypot <flonum>)
    (syntax-rules ()
      ((_ ?fl1 ?fl2)
       (flhypot ?fl1 ?fl2))))

  ;; methods: min and max
  (method-syntax (brace max <flonum>)
    (syntax-rules ()
      ((_ ?fl1 ?fl ...)
       (flmax ?fl1 ?fl ...))))

  (method-syntax (brace min <flonum>)
    (syntax-rules ()
      ((_ ?fl1 ?fl ...)
       (flmin ?fl1 ?fl ...))))

  #| end of label |# )

(define-builtin-label <integer-flonum>
  (parent <flonum>)
  (predicate $flonum-integer?))

(define-builtin-label <rational-flonum>
  (parent <flonum>)
  (predicate $flonum-rational?))


;;;; built-in types: procedure objects

;; (define-builtin-label <procedure>
;;   (predicate procedure?))


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

   ;;Numeric object identification.  Order does matter here!!!
   ;;
   ;;Notice that it is almost useless  to apply NUMBER? first and have a
   ;;nested COND syntax with the various numeric predicates, as in:
   ;;
   ;;  ((number? obj)	(cond ((fixnum? obj) ...) ...))
   ;;
   ;;because  NUMBER?  itself  is  implemented as  a  series of  numeric
   ;;predicates applications.
   ;;
   ((fixnum?		obj)
    ;;We  do  not  test   for  either  "<nonnegative-fixnum>"  or
    ;;"<nonpositive-fixnum>".
    (cond (($fxpositive? obj)	(tag-unique-identifiers <positive-fixnum>))
	  (($fxnegative? obj)	(tag-unique-identifiers <negative-fixnum>))
	  (else			(tag-unique-identifiers <fixnum>))))
   ((bignum?		obj)	(if ($bignum-positive? obj)
				    (tag-unique-identifiers <positive-bignum>)
				  (tag-unique-identifiers <negative-bignum>)))
   ((flonum?		obj)	(cond (($flonum-integer? obj)
				       (tag-unique-identifiers <integer-flonum>))
				      (($flonum-rational? obj)
				       (tag-unique-identifiers <rational-flonum>))
				      (else
				       (tag-unique-identifiers <flonum>))))
   ((ratnum?		obj)	(tag-unique-identifiers <ratnum>))
   ((cflonum?		obj)	(tag-unique-identifiers <cflonum>))
   ((compnum?		obj)	(tag-unique-identifiers <compnum>))

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
  (fields (mutable (brace %uid	<symbol>)))

  (virtual-fields
   (immutable (brace uid <symbol>)
	      (lambda/tags ((brace O <class>))
		(or (O $%uid)
		    (receive-and-return (sym)
			(gensym)
		      (set!/tags (O $%uid) sym)))))
   (immutable (brace hash <fixnum>)
	      (lambda/tags ((brace O <class>))
		;;We memoize the hash value  in the "value" field of the
		;;symbol's data structure.
		(if ($unbound-object? ($symbol-value (O uid)))
		    (receive-and-return (H)
			(symbol-hash (O $%uid))
		      ($set-symbol-value! (O $%uid) H))
		  ($symbol-value (O $%uid))))))

  (method (putprop (brace O <class>) (brace key <symbol>) value)
    (putprop (O uid) key value))

  (method (getprop (brace O <class>) (brace key <symbol>))
    (getprop (O uid) key))

  (method (remprop (brace O <class>) (brace key <symbol>))
    (remprop (O uid) key))

  (method (property-list (brace O <class>))
    (property-list (O uid)))

  #| end of mixin |# )


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'aux.method-syntax 'scheme-indent-function 1)
;; eval: (put 'aux.method 'scheme-indent-function 1)
;; End:
