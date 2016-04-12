;; scheme-object-types.scm --
;;
;; Definitions of built-in Scheme object types.


;;;; core syntactic binding descriptors: built-in Scheme object types

(define-scheme-type <top>
    #f
  (constructor <top>-constructor)
  (predicate <top>-type-predicate)
  (hash-function object-hash))

(define-scheme-type <no-return>
    #f
  (predicate always-false)
  (hash-function object-hash))


;;;; standalone object types

(define-scheme-type <void>
    <top>
  (constructor void)
  (predicate void-object?)
  (hash-function void-hash))

(define-scheme-type <eof>
    <top>
  (constructor eof-object)
  (predicate eof-object?)
  (hash-function eof-object-hash))

(define-scheme-type <would-block>
    <top>
  (constructor would-block-object)
  (predicate would-block-object?)
  (hash-function would-block-hash))

(define-scheme-type <boolean>
    <top>
  (constructor <boolean>-constructor)
  (predicate boolean?)
  (hash-function boolean-hash))

(define-scheme-type <true>
    <boolean>
  (constructor #t)
  (predicate true?))

(define-scheme-type <false>
    <boolean>
  (constructor #t)
  (predicate false?))

(define-scheme-type <char>
    <top>
  (constructor integer->char)
  (predicate char?)
  (hash-function char-hash)
  (methods
   (string		string)
   (integer		char->integer)
   (fixnum		char->fixnum)))

(define-scheme-type <symbol>
    <top>
  (constructor string->symbol)
  (predicate symbol?)
  (hash-function symbol-hash)
  (methods
   (string		symbol->string)
   (bound?		symbol-bound?)
   (value		<symbol>-value)
   (putprop		putprop)
   (getprop		getprop)
   (remprop		remprop)
   (property-list	property-list)))

(define-scheme-type <gensym>
    <symbol>
  (constructor gensym)
  (predicate gensym?))

(define-scheme-type <keyword>
    <top>
  (constructor symbol->keyword)
  (predicate keyword?)
  (hash-function keyword-hash)
  (methods
   (symbol		keyword->symbol)
   (string		keyword->string)))

(define-scheme-type <pointer>
    <top>
  (constructor integer->pointer)
  (predicate pointer?)
  (hash-function pointer-hash)
  (methods
   (null?		pointer-null?)
   (integer		pointer->integer)
   (=			pointer=?)
   (!=			pointer!=?)
   (<			pointer<?)
   (>			pointer>?)
   (<=			pointer<=?)
   (>=			pointer>=?)
   (add			pointer-add)
   (diff		pointer-diff)
   (clone		pointer-clone)
   (set-null!		set-pointer-null!)))

(define-scheme-type <transcoder>
    <top>
  (constructor make-transcoder)
  (predicate transcoder?)
  (hash-function transcoder-hash)
  (methods
   (codec		transcoder-codec)
   (eol-style		transcoder-eol-style)
   (handling-mode	transcoder-error-handling-mode)))


;;;; procedures

(define-scheme-type <procedure>
    <top>
  (predicate procedure?))


;;; numeric types

(define-scheme-type <number>
    <top>
  (predicate number?)
  (hash-function object-hash))

(define-scheme-type <complex>
    <number>
  (constructor make-rectangular)
  (predicate complex?))

(define-scheme-type <real-valued>
    <complex>
  (predicate real-valued?))

(define-scheme-type <real>
    <real-valued>
  (predicate real?))

(define-scheme-type <rational-valued>
    <real>
  (predicate rational-valued?))

(define-scheme-type <rational>
    <rational-valued>
  (predicate  rational?))

;;This "<integer-valued>" is a bit orphan: it is excluded from the hierarchy.
;;
(define-scheme-type <integer-valued>
    <rational-valued>
  (predicate integer-valued?))

;;Notice that "<integer>" is a "<rational>", not an "<integer-valued>".
;;
(define-scheme-type <integer>
    <rational>
  (predicate integer?))

(define-scheme-type <exact-integer>
    <integer>
  (predicate exact-integer?)
  (hash-function exact-integer-hash))

(define-scheme-type <fixnum>
    <exact-integer>
  (constructor #t)
  (predicate fixnum?)
  (hash-function fixnum-hash))

(define-scheme-type <flonum>
    <real>
  (constructor #t)
  (predicate flonum?)
  (hash-function flonum-hash))

(define-scheme-type <ratnum>
    <rational>
  (constructor #t)
  (predicate ratnum?))

(define-scheme-type <bignum>
    <exact-integer>
  (constructor #t)
  (predicate bignum?))

(define-scheme-type <compnum>
    <complex>
  (constructor #t)
  (predicate compnum?))

(define-scheme-type <cflonum>
    <complex>
  (constructor #t)
  (predicate cflonum?))

;;; --------------------------------------------------------------------

(define-scheme-type <zero-fixnum>
    <fixnum>
  (constructor #t)
  (predicate zero-fixnum?))

(define-scheme-type <positive-fixnum>
    <fixnum>
  (constructor #t)
  (predicate positive-fixnum?))

(define-scheme-type <negative-fixnum>
    <fixnum>
  (constructor #t)
  (predicate negative-fixnum?))

;;; --------------------------------------------------------------------

(define-scheme-type <positive-bignum>
    <bignum>
  (constructor #t)
  (predicate positive-bignum?))

(define-scheme-type <negative-bignum>
    <bignum>
  (constructor #t)
  (predicate negative-bignum?))

;;; --------------------------------------------------------------------

(define-scheme-type <positive-ratnum>
    <ratnum>
  (constructor #t)
  (predicate positive-ratnum?))

(define-scheme-type <negative-ratnum>
    <ratnum>
  (constructor #t)
  (predicate negative-ratnum?))

;;; --------------------------------------------------------------------

(define-scheme-type <positive-flonum>
    <flonum>
  (constructor #t)
  (predicate positive-flonum?))

(define-scheme-type <negative-flonum>
    <flonum>
  (constructor #t)
  (predicate negative-flonum?))

(define-scheme-type <positive-zero-flonum>
    <positive-flonum>
  (constructor #t)
  (predicate positive-zero-flonum?))

(define-scheme-type <negative-zero-flonum>
    <negative-flonum>
  (constructor #t)
  (predicate negative-zero-flonum?))

;;; --------------------------------------------------------------------

(define-scheme-type <exact-compnum>
    <compnum>
  (constructor #t)
  (predicate exact-compnum?))

(define-scheme-type <inexact-compnum>
    <compnum>
  (constructor #t)
  (predicate inexact-compnum?))


;;;; compound types

(define-scheme-type <string>
    <top>
  (constructor string)
  (predicate string?)
  (hash-function string-hash)
  (methods
   (empty?			string-empty?)

   (copy			string-copy)
   (append			string-append)

   (length			string-length)
   (for-each			<string>-for-each)
   (ref				string-ref)
   (set!			string-set!)
   (fill!			string-fill!)

   (ascii-encoded?		ascii-encoded-string?)
   (latin1-encoded?		latin1-encoded-string?)
   (octets-encoded?		octets-encoded-string?)
   (uri-encoded			uri-encoded-string?)
   (pencent-encoded?		percent-encoded-string?)

   (<=				string<=?)
   (<				string<?)
   (=				string=?)
   (>=				string>=?)
   (>				string>?)

   (ci<=			string-ci<=?)
   (ci<				string-ci<?)
   (ci=				string-ci=?)
   (ci>=			string-ci>=?)
   (ci>				string-ci>?)

   (titlecase			string-titlecase)
   (upcase			string-upcase)
   (downcase			string-downcase)
   (foldcase			string-foldcase)

   (normalize-nfc		string-normalize-nfc)
   (normalize-nfd		string-normalize-nfd)
   (normalize-nfkc		string-normalize-nfkc)
   (normalize-nfkd		string-normalize-nfkd)

   (flonum			string->flonum)
   (number			string->number)
   (utf8			string->utf8)
   (utf16			string->utf16)
   (utf32			string->utf32)
   (bytevector			string->bytevector)

   (ascii			string->ascii)
   (latin1			string->latin1)
   (octets			string->octets)
   (percent-encoding		string->percent-encoding)
   (uri-encoding		string->uri-encoding)
   (utf16be			string->utf16be)
   (utf16le			string->utf16le)
   (utf16n			string->utf16n)
   (base64->bytevector		string-base64->bytevector)
   (hex->bytevector		string-hex->bytevector)

   (symbol			string->symbol)
   (keyword			string->keyword)
   (list			string->list)
   ))

(define-scheme-type <vector>
    <top>
  (constructor vector)
  (predicate vector?)
  (methods
   (empty?			vector-empty?)
   (length			vector-length)
   (ref				vector-ref)
   (set!			vector-set!)
   (fill!			vector-fill!)
   (append			vector-append)
   (subvector			subvector)
   (resize			vector-resize)
   (map				<vector>-map)
   (for-each			<vector>-for-each)
   (for-all			<vector>-for-all)
   (exists			<vector>-exists)
   (find			<vector>-find)
   (fold-left			<vector>-fold-left)
   (fold-right			<vector>-fold-right)
   (sort			<vector>-sort)
   (sort!			<vector>-sort!)
   (list			vector->list)))

(define-scheme-type <empty-vector>
    <vector>
  (constructor <empty-vector>-constructor)
  (predicate <empty-vector>-type-predicate))

(define-scheme-type <bytevector>
    <top>
  (constructor make-bytevector)
  (predicate bytevector?)
  (hash-function bytevector-hash))

;;; --------------------------------------------------------------------

(define-scheme-type <hashtable>
    <top>
  (predicate hashtable?))

(define-scheme-type <hashtable-eq>
    <hashtable>
  (constructor make-eq-hashtable)
  (predicate hashtable-eq?))

(define-scheme-type <hashtable-eqv>
    <hashtable>
  (constructor make-eqv-hashtable)
  (predicate hashtable-eqv?))

(define-scheme-type <hashtable-equal>
    <hashtable>
  (constructor make-hashtable)
  (predicate hashtable-equiv?))

;;; --------------------------------------------------------------------

(define-scheme-type <code>
    <top>
  (predicate code?))


;;;; records and structs

(define-scheme-type <struct>
    <top>
  (predicate struct?)
  (hash-function struct-hash))

(define-scheme-type <struct-type-descriptor>
    <struct>
  (constructor make-struct-type)
  (predicate struct-type-descriptor?))

;;; --------------------------------------------------------------------

(define-scheme-type <record>
    <struct>
  (predicate record?)
  (hash-function record-hash))

(define-scheme-type <record-type-descriptor>
    <struct>
  (constructor make-record-type-descriptor)
  (predicate record-type-descriptor?))

(define-scheme-type <record-constructor-descriptor>
    <struct>
  (constructor make-record-constructor-descriptor)
  (predicate record-constructor-descriptor?))

;;; --------------------------------------------------------------------

(define-scheme-type <opaque-record>
    <top>
  (predicate always-false)
  (hash-function record-hash))

;;; --------------------------------------------------------------------

;;This is the root of all the  condition object types, both simple and compound.  All
;;the simple condition types are derived from "&condition".
;;
(define-scheme-type <condition>
    <record>
  (predicate condition?)
  (methods
   (print	print-condition)))

(define-scheme-type <compound-condition>
    <condition>
  (constructor condition)
  (predicate compound-condition?))

;;; --------------------------------------------------------------------

(define-scheme-type <promise>
    <record>
  (constructor make-promise)
  (predicate promise?)
  (methods
   (force	force)))

(define-scheme-type <enum-set>
    <struct>
  (constructor make-enumeration)
  (predicate enum-set?)
  (methods
   (list		enum-set->list)
   (complement		enum-set-complement)
   (constructor		enum-set-constructor)
   (difference		enum-set-difference)
   (indexer		enum-set-indexer)
   (intersection	enum-set-intersection)
   (member?		enum-set-member?)
   (projection		enum-set-projection)
   (subset?		enum-set-subset?)
   (union		enum-set-union)
   (universe		enum-set-universe)
   (=?			enum-set=?)))

;;; --------------------------------------------------------------------

(define-scheme-type <utsname>
    <struct>
  (constructor uname)
  (predicate utsname?)
  (methods
   (sysname		utsname-sysname)
   (nodename		utsname-nodename)
   (release		utsname-release)
   (version		utsname-version)
   (machine		utsname-machine)))

;;; --------------------------------------------------------------------

(define-scheme-type <memory-block>
    <struct>
  (constructor make-memory-block)
  (predicate memory-block?)
  (methods
   (pointer		memory-block-pointer)
   (size		memory-block-size)
   (reset		memory-block-reset)))

;;; --------------------------------------------------------------------

(define-scheme-type <stats>
    <struct>
  (predicate stats?)
  (methods
   (user-secs		stats-user-secs)
   (user-usecs		stats-user-usecs)
   (sys-secs		stats-sys-secs)
   (sys-usecs		stats-sys-usecs)
   (real-secs		stats-real-secs)
   (real-usecs		stats-real-usecs)
   (collection-id	stats-collection-id)
   (gc-user-secs	stats-gc-user-secs)
   (gc-user-usecs	stats-gc-user-usecs)
   (gc-sys-secs		stats-gc-sys-secs)
   (gc-sys-usecs	stats-gc-sys-usecs)
   (gc-real-secs	stats-gc-real-secs)
   (gc-real-usecs	stats-gc-real-usecs)
   (bytes-minor		stats-bytes-minor)
   (bytes-major		stats-bytes-major)))

(define-scheme-type <reader-annotation>
    <struct>
  (constructor get-annotated-datum)
  (predicate reader-annotation?)
  (methods
   (expression			reader-annotation-expression)
   (stripped			reader-annotation-stripped)
   (source			reader-annotation-source)
   (textual-position		reader-annotation-textual-position)))

(define-scheme-type <time>
    <struct>
  (constructor current-time)
  (predicate time?)
  (methods
   (second		time-second)
   (nanosecond		time-nanosecond)))

(define-scheme-type <scheme-type-descriptor>
    <struct>
  (predicate scheme-type-descriptor?)
  (methods
   (name				scheme-type-descriptor-name)
   (parent				scheme-type-descriptor-parent)
   (type-predicate			scheme-type-descriptor-type-predicate)
   (equality-predicate			scheme-type-descriptor-equality-predicate)
   (comparison-procedure		scheme-type-descriptor-comparison-procedure)
   (hash-function			scheme-type-descriptor-hash-function)
   (uids-list				scheme-type-descriptor-uids-list)
   (method-retriever			scheme-type-descriptor-method-retriever)))


;;;; input/output ports

(define-scheme-type <port>
    <top>
  (predicate port?)
  (hash-function port-hash))

;;; --------------------------------------------------------------------

(define-scheme-type <input-port>
    <port>
  (predicate input-port?))

(define-scheme-type <output-port>
    <port>
  (predicate output-port?))

(define-scheme-type <input/output-port>
    <port>
  (predicate input/output-port?))

;;; --------------------------------------------------------------------

(define-scheme-type <textual-port>
    <port>
  (predicate textual-port?))

(define-scheme-type <binary-port>
    <port>
  (predicate binary-port?))

;;; --------------------------------------------------------------------

(define-scheme-type <textual-input-port>
    <input-port>
  (predicate textual-input-port?))

(define-scheme-type <textual-output-port>
    <output-port>
  (predicate textual-output-port?))

(define-scheme-type <textual-input/output-port>
    <input/output-port>
  (predicate textual-input/output-port?))

;;; --------------------------------------------------------------------

(define-scheme-type <binary-input-port>
    <input-port>
  (predicate binary-input-port?))

(define-scheme-type <binary-output-port>
    <output-port>
  (predicate binary-output-port?))

(define-scheme-type <binary-input/output-port>
    <input/output-port>
  (predicate binary-input/output-port?))


;;;; list types

(define-scheme-type <list>
    <top>
  (constructor list)
  (predicate list?)
  (methods
   (car		car)
   (cdr		cdr)))

(define-scheme-type <null>
    <list>
  (constructor <null>-constructor)
  (predicate null?))

(define-scheme-type <pair>
    <top>
  (constructor cons)
  (predicate pair?)
  (methods
   (car		car)
   (cdr		cdr)))

;;; --------------------------------------------------------------------

(define-scheme-type <ipair>
    <struct>
  (constructor ipair)
  (predicate ipair?)
  (methods
   (car		icar)
   (cdr		icdr)))


;;;; done

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
