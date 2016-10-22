;; scheme-object-types.scm --
;;
;; Definitions of built-in Scheme object types.


;;;; core syntactic binding descriptors: built-in Scheme object types

(define-scheme-type <top>
    #f
  (constructor <top>-constructor)
  (type-predicate <top>-type-predicate)
  (equality-predicate equal?)
  (hash-function object-hash))

(define-scheme-type <untyped>
    #f
  (constructor <untyped>-constructor)
  (type-predicate <untyped>-type-predicate)
  (equality-predicate equal?)
  (hash-function object-hash))

(define-scheme-type <void>
    #f
  (constructor void)
  (type-predicate void-object?)
  (hash-function void-hash))

(define-scheme-type <bottom>
    #f
  (type-predicate always-false)
  (hash-function object-hash))


;;;; standalone object types

(define-scheme-type <eof>
    <top>
  (constructor eof-object)
  (type-predicate eof-object?)
  (hash-function eof-object-hash))

(define-scheme-type <would-block>
    <top>
  (constructor would-block-object)
  (type-predicate would-block-object?)
  (hash-function would-block-hash))

(define-scheme-type <boolean>
    <top>
  (constructor <boolean>-constructor)
  (type-predicate boolean?)
  (equality-predicate eq?)
  (comparison-procedure compar-boolean)
  (hash-function boolean-hash))

(define-scheme-type <true>
    <boolean>
  (constructor #t)
  (type-predicate true?)
  (equality-predicate eq?)
  (comparison-procedure compar-boolean)
  (hash-function boolean-hash))

(define-scheme-type <false>
    <boolean>
  (constructor #t)
  (type-predicate false?))

(define-scheme-type <char>
    <top>
  (constructor integer->char)
  (type-predicate char?)
  (hash-function char-hash)
  (comparison-procedure compar-char)
  (equality-predicate char=?)
  (methods
   (string		string)
   (integer		char->integer)
   (fixnum		char->fixnum)
   ;;
   (<=			char<=?)
   (<			char<?)
   (=			char=?)
   (!=			char!=?)
   (>=			char>=?)
   (>			char>?)
   ;;
   (ci<=		char-ci<=?)
   (ci<			char-ci<?)
   (ci=			char-ci=?)
   (ci!=		char-ci!=?)
   (ci>=		char-ci>=?)
   (ci>			char-ci>?)
   ;;
   (min			chmin)
   (max			chmax)
   ;;
   (downcase		char-downcase)
   (foldcase		char-foldcase)
   (titlecase		char-titlecase)
   (upcase		char-upcase)
   (general-category	char-general-category)
   (alphabetic?		char-alphabetic?)
   (lower-case?		char-lower-case?)
   (numeric?		char-numeric?)
   (title-case?		char-title-case?)
   (upper-case?		char-upper-case?)
   (whitespace?		char-whitespace?)))

(define-scheme-type <symbol>
    <top>
  (constructor string->symbol)
  (type-predicate symbol?)
  (hash-function symbol-hash)
  (comparison-procedure compar-symbol)
  (equality-predicate eq?)
  (methods
   (string		symbol->string)
   (bound?		symbol-bound?)
   (value		<symbol>-value)
   ;;
   (=			symbol=?)
   (!=			symbol!=?)
   (<			symbol<?)
   (>			symbol>?)
   (<=			symbol<=?)
   (>=			symbol>=?)
   (max			symbol-max)
   (min			symbol-min)
   ;;
   (putprop		putprop)
   (getprop		getprop)
   (remprop		remprop)
   (property-list	property-list)))

(define-scheme-type <gensym>
    <symbol>
  (constructor gensym)
  (type-predicate gensym?)
  (equality-predicate eq?))

(define-scheme-type <keyword>
    <top>
  (constructor symbol->keyword)
  (type-predicate keyword?)
  (hash-function keyword-hash)
  (equality-predicate keyword=?)
  (methods
   (symbol		keyword->symbol)
   (string		keyword->string)))

(define-scheme-type <pointer>
    <top>
  (constructor integer->pointer)
  (type-predicate pointer?)
  (equality-predicate pointer=?)
  (comparison-procedure compar-transcoder)
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
  (type-predicate transcoder?)
  (equality-predicate transcoder=?)
  (comparison-procedure compar-transcoder)
  (hash-function transcoder-hash)
  (methods
   (codec		transcoder-codec)
   (eol-style		transcoder-eol-style)
   (handling-mode	transcoder-error-handling-mode)))


;;;; procedures

(define-scheme-type <procedure>
    <top>
  (type-predicate procedure?))


;;; numeric types

(define-scheme-type <number>
    <top>
  (type-predicate number?)
  (equality-predicate =)
  (hash-function object-hash))

(define-scheme-type <complex>
    <number>
  (constructor make-rectangular)
  (type-predicate complex?)
  (equality-predicate =))

(define-scheme-type <real-valued>
    <complex>
  (type-predicate real-valued?)
  (equality-predicate =))

(define-scheme-type <real>
    <real-valued>
  (type-predicate real?)
  (equality-predicate =)
  (comparison-procedure compar-real))

(define-scheme-type <rational-valued>
    <real>
  (type-predicate rational-valued?)
  (equality-predicate =))

(define-scheme-type <rational>
    <rational-valued>
  (type-predicate  rational?)
  (equality-predicate =))

;;This "<integer-valued>" is a bit orphan: it is excluded from the hierarchy.
;;
(define-scheme-type <integer-valued>
    <rational-valued>
  (type-predicate integer-valued?)
  (equality-predicate =))

;;Notice that "<integer>" is a "<rational>", not an "<integer-valued>".
;;
(define-scheme-type <integer>
    <rational>
  (type-predicate integer?))

(define-scheme-type <exact-integer>
    <integer>
  (type-predicate exact-integer?)
  (hash-function exact-integer-hash)
  (equality-predicate =)
  (comparison-procedure compar-exact-integer))

(define-scheme-type <fixnum>
    <exact-integer>
  (constructor #t)
  (type-predicate fixnum?)
  (hash-function fixnum-hash)
  (comparison-procedure compar-fixnum)
  (equality-predicate fx=?))

(define-scheme-type <flonum>
    <real>
  (constructor #t)
  (type-predicate flonum?)
  (equality-predicate fl=?)
  (comparison-procedure compar-flonum)
  (hash-function flonum-hash))

(define-scheme-type <ratnum>
    <rational>
  (constructor #t)
  (type-predicate ratnum?)
  (equality-predicate =)
  (comparison-procedure compar-ratnum)
  #;(hash-function ratnum-hash))

(define-scheme-type <bignum>
    <exact-integer>
  (constructor #t)
  (type-predicate bignum?)
  (equality-predicate =)
  (comparison-procedure compar-bignum))

(define-scheme-type <compnum>
    <complex>
  (constructor #t)
  (type-predicate compnum?)
  (equality-predicate =))

(define-scheme-type <cflonum>
    <complex>
  (constructor #t)
  (type-predicate cflonum?)
  (equality-predicate =))

;;; --------------------------------------------------------------------

(define-scheme-type <zero-fixnum>
    <fixnum>
  (constructor #t)
  (type-predicate zero-fixnum?)
  (equality-predicate fx=?))

(define-scheme-type <positive-fixnum>
    <fixnum>
  (constructor #t)
  (type-predicate positive-fixnum?)
  (equality-predicate fx=?))

(define-scheme-type <negative-fixnum>
    <fixnum>
  (constructor #t)
  (type-predicate negative-fixnum?)
  (equality-predicate fx=?))

;;; --------------------------------------------------------------------

(define-scheme-type <positive-bignum>
    <bignum>
  (constructor #t)
  (type-predicate positive-bignum?)
  (equality-predicate =))

(define-scheme-type <negative-bignum>
    <bignum>
  (constructor #t)
  (type-predicate negative-bignum?)
  (equality-predicate =))

;;; --------------------------------------------------------------------

(define-scheme-type <positive-ratnum>
    <ratnum>
  (constructor #t)
  (type-predicate positive-ratnum?)
  (equality-predicate =))

(define-scheme-type <negative-ratnum>
    <ratnum>
  (constructor #t)
  (type-predicate negative-ratnum?)
  (equality-predicate =))

;;; --------------------------------------------------------------------

(define-scheme-type <zero-flonum>
    <flonum>
  (constructor #t)
  (type-predicate zero-flonum?)
  (equality-predicate fl=?))

(define-scheme-type <positive-zero-flonum>
    <zero-flonum>
  (constructor #t)
  (type-predicate positive-zero-flonum?)
  (equality-predicate fl=?))

(define-scheme-type <negative-zero-flonum>
    <zero-flonum>
  (constructor #t)
  (type-predicate negative-zero-flonum?)
  (equality-predicate fl=?))

;;;

(define-scheme-type <positive-flonum>
    <flonum>
  (constructor #t)
  (type-predicate positive-flonum?)
  (equality-predicate fl=?))

(define-scheme-type <negative-flonum>
    <flonum>
  (constructor #t)
  (type-predicate negative-flonum?)
  (equality-predicate fl=?))

;;; --------------------------------------------------------------------

(define-scheme-type <exact-compnum>
    <compnum>
  (constructor #t)
  (type-predicate exact-compnum?)
  (equality-predicate =))

(define-scheme-type <inexact-compnum>
    <compnum>
  (constructor #t)
  (type-predicate inexact-compnum?)
  (equality-predicate =))

(define-scheme-type <zero-compnum>
    <inexact-compnum>
  (constructor #t)
  (type-predicate zero-compnum?)
  (equality-predicate =))

(define-scheme-type <non-zero-inexact-compnum>
    <inexact-compnum>
  (constructor #t)
  (type-predicate non-zero-inexact-compnum?)
  (equality-predicate =))

;;; --------------------------------------------------------------------

(define-scheme-type <zero-cflonum>
    <cflonum>
  (constructor #t)
  (type-predicate zero-cflonum?)
  (equality-predicate =))

(define-scheme-type <non-zero-cflonum>
    <cflonum>
  (constructor #t)
  (type-predicate non-zero-cflonum?)
  (equality-predicate =))


;;;; strings

(define-scheme-type <string>
    <top>
  (constructor string)
  (type-predicate string?)
  (equality-predicate string=?)
  (comparison-procedure compar-string)
  (hash-function string-hash)
  (methods
   (empty?			string-empty?)

   (copy			string-copy)
   (append			string-append)

   (length			string-length)
   (for-each			<string>-for-each)
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

(define-scheme-type <empty-string>
    <string>
  (constructor <empty-string>-constructor)
  (type-predicate empty-string?)
  (equality-predicate string=?)
  (comparison-procedure compar-string))

(define-scheme-type <nestring>
    <string>
  (constructor <nestring>-constructor)
  (type-predicate nestring?)
  (equality-predicate string=?)
  (comparison-procedure compar-string)
  (methods
   (ref				string-ref)
   (set!			string-set!)))


;;;; compound types

(define-scheme-type <vector>
    <top>
  (constructor vector)
  (type-predicate vector?)
  (equality-predicate equal?)
  (methods
   (empty?			vector-empty?)
   (length			vector-length)
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
  (type-predicate <empty-vector>-type-predicate)
  (equality-predicate equal?))

(define-scheme-type <nevector>
    <vector>
  (constructor <nevector>-constructor)
  (type-predicate <nevector>-type-predicate)
  (equality-predicate equal?)
  (methods
   (ref				vector-ref)
   (set!			vector-set!)))

(define-scheme-type <bytevector>
    <top>
  (constructor make-bytevector)
  (type-predicate bytevector?)
  (hash-function bytevector-hash)
  (equality-predicate bytevector=?)
  (methods
   (length		bytevector-length)
   (=			bytevector=?)
   (!=			bytevector!=?)))

(define-scheme-type <empty-bytevector>
    <bytevector>
  (constructor <empty-bytevector>-constructor)
  (type-predicate <empty-bytevector>-type-predicate)
  (equality-predicate bytevector=?))

(define-scheme-type <nebytevector>
    <bytevector>
  (constructor <nebytevector>-constructor)
  (type-predicate <nebytevector>-type-predicate)
  (equality-predicate bytevector=?))

;;; --------------------------------------------------------------------

(define-scheme-type <code>
    <top>
  (type-predicate code?))


;;;; records and structs

(define-scheme-type <struct>
    <top>
  (type-predicate struct?)
  (equality-predicate struct=?)
  (hash-function struct-hash))

(define-scheme-type <struct-type-descriptor>
    <struct>
  (constructor make-struct-type)
  (type-predicate struct-type-descriptor?)
  (equality-predicate struct=?))

;;; --------------------------------------------------------------------

(define-scheme-type <record>
    <struct>
  (type-predicate record?)
  (equality-predicate record=?)
  (hash-function record-hash))

(define-scheme-type <record-type-descriptor>
    <struct>
  (constructor make-record-type-descriptor)
  (type-predicate record-type-descriptor?)
  (equality-predicate struct=?))

(define-scheme-type <record-constructor-descriptor>
    <struct>
  (constructor make-record-constructor-descriptor)
  (type-predicate record-constructor-descriptor?)
  (equality-predicate struct=?))

;;; --------------------------------------------------------------------

(define-scheme-type <opaque-record>
    <top>
  (type-predicate always-false)
  (equality-predicate record=?)
  (hash-function record-hash))

;;; --------------------------------------------------------------------

(define-scheme-type <tcbucket>
    <top>
  (type-predicate tcbucket?))

(define-scheme-type <hashtable>
    <struct>
  (type-predicate hashtable?))

(define-scheme-type <hashtable-eq>
    <hashtable>
  (constructor make-eq-hashtable)
  (type-predicate hashtable-eq?))

(define-scheme-type <hashtable-eqv>
    <hashtable>
  (constructor make-eqv-hashtable)
  (type-predicate hashtable-eqv?))

(define-scheme-type <hashtable-equal>
    <hashtable>
  (constructor make-hashtable)
  (type-predicate hashtable-equiv?))

;;; --------------------------------------------------------------------

;;This is the root of all the  condition object types, both simple and compound.  All
;;the simple condition types are derived from "&condition".
;;
(define-scheme-type <condition>
    <record>
  (type-predicate condition?)
  (methods
   (print	print-condition)))

(define-scheme-type <compound-condition>
    <condition>
  (constructor condition)
  (type-predicate compound-condition?))

;;; --------------------------------------------------------------------

(define-scheme-type <promise>
    <record>
  (constructor make-promise)
  (type-predicate promise?)
  (methods
   (force	force)))

(define-scheme-type <enum-set>
    <struct>
  (constructor make-enumeration)
  (type-predicate enum-set?)
  (equality-predicate enum-set=?)
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
  (type-predicate utsname?)
  (equality-predicate struct=?)
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
  (type-predicate memory-block?)
  (equality-predicate struct=?)
  (methods
   (pointer		memory-block-pointer)
   (size		memory-block-size)
   (reset		memory-block-reset)))

;;; --------------------------------------------------------------------

(define-scheme-type <stats>
    <struct>
  (type-predicate stats?)
  (equality-predicate struct=?)
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
  (type-predicate reader-annotation?)
  (equality-predicate struct=?)
  (methods
   (expression			reader-annotation-expression)
   (stripped			reader-annotation-stripped)
   (source			reader-annotation-source)
   (textual-position		reader-annotation-textual-position)))

;;; --------------------------------------------------------------------
;;; unique objects

(define-scheme-type <sentinel>
    <struct>
  (constructor sentinel)
  (type-predicate sentinel?)
  (equality-predicate eq?))


;;;; input/output ports

(define-scheme-type <port>
    <top>
  (type-predicate port?)
  (equality-predicate eq?)
  (hash-function port-hash))

;;; --------------------------------------------------------------------

(define-scheme-type <input-port>
    <port>
  (type-predicate input-port?)
  (equality-predicate eq?))

(define-scheme-type <output-port>
    <port>
  (type-predicate output-port?)
  (equality-predicate eq?))

(define-scheme-type <input/output-port>
    <port>
  (type-predicate input/output-port?)
  (equality-predicate eq?))

;;; --------------------------------------------------------------------

(define-scheme-type <textual-input-only-port>
    <input-port>
  (type-predicate textual-input-only-port?)
  (equality-predicate eq?))

(define-scheme-type <textual-output-only-port>
    <output-port>
  (type-predicate textual-output-only-port?)
  (equality-predicate eq?))

(define-scheme-type <textual-input/output-port>
    <input/output-port>
  (type-predicate textual-input/output-port?)
  (equality-predicate eq?))

;;; --------------------------------------------------------------------

(define-scheme-type <binary-input-only-port>
    <input-port>
  (type-predicate binary-input-only-port?)
  (equality-predicate eq?))

(define-scheme-type <binary-output-only-port>
    <output-port>
  (type-predicate binary-output-only-port?)
  (equality-predicate eq?))

(define-scheme-type <binary-input/output-port>
    <input/output-port>
  (type-predicate binary-input/output-port?)
  (equality-predicate eq?))


;;;; list types

(define-scheme-type <list>
    <top>
  (constructor list)
  (type-predicate list?)
  (equality-predicate equal?)
  (methods
   (length	length)))

(define-scheme-type <nelist>
    <list>
  (constructor <nelist>-constructor)
  (type-predicate <nelist>-type-predicate)
  (equality-predicate equal?)
  (methods
   (car		car)
   (cdr		cdr)))

(define-scheme-type <null>
    <list>
  (constructor #t)
  (type-predicate null?)
  (equality-predicate eq?))

(define-scheme-type <pair>
    <top>
  (constructor cons)
  (type-predicate pair?)
  (equality-predicate equal?)
  (methods
   (car		car)
   (cdr		cdr)))

;;; --------------------------------------------------------------------

(define-scheme-type <ipair>
    <struct>
  (constructor ipair)
  (type-predicate ipair?)
  ;;EQUAL? has hard-coded special support for immutable pairs.
  (equality-predicate equal?)
  (methods
   (car		icar)
   (cdr		icdr)))


;;;; done

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
