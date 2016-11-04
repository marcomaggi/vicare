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
  (comparison-procedure compar-pointer)
  (hash-function pointer-hash)
  (methods
   (integer		pointer->integer)
   (null?		pointer-null?)
   (set-null!		set-pointer-null!)
   (add			pointer-add)
   (diff		pointer-diff)
   (clone		pointer-clone)
;;
   (=			pointer=?)
   (!=			pointer!=?)
   (<			pointer<?)
   (>			pointer>?)
   (<=			pointer<=?)
   (>=			pointer>=?)))

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
  (constructor #t)
  (type-predicate number?)
  ;;This equality predicate is inherited as default for all the numeric types.
  (equality-predicate =)
  ;;This hash function is inherited as default for all the numeric types.
  (hash-function number-hash)
  (methods
   (=			=)
   (!=			!=)
   ;;
   (+			+)
   (-			-)
   (*			*)
   (/			/)
   (add1		add1)
   (sub1		sub1)
   ;;
   (zero?		zero?)
   (nan?		nan?)
   (odd?		odd?)
   (even?		even?)
   (finite?		finite?)
   (infinite?		infinite?)
   ;;
   (exact		exact)
   (inexact		inexact)
   ;;
   (magnitude		magnitude)
   (angle		angle)
   (real-part		real-part)
   (imag-part		imag-part)
   (complex-conjugate	complex-conjugate)
   ;;
   (exp			exp)
   (log			log)
   (expt		expt)
   (square		square)
   (cube		cube)
   (sqrt		sqrt)
   (cbrt		cbrt)
   ;;
   (sin			sin)
   (cos			cos)
   (tan			tan)
   (asin		asin)
   (acos		acos)
   (atan		atan)
   ;;
   (sinh		sinh)
   (cosh		cosh)
   (tanh		tanh)
   (asinh		asinh)
   (acosh		acosh)
   (atanh		atanh)
   #| end of METHODS |# ))

(define-scheme-type <complex>
    <number>
  (constructor make-rectangular)
  (type-predicate complex?))

(define-scheme-type <real-valued>
    <complex>
  (constructor #t)
  (type-predicate real-valued?))

(define-scheme-type <real>
    <real-valued>
  (constructor #t)
  (type-predicate real?)
  ;;This comparison procedure is inherited as default for all the real numeric types.
  (comparison-procedure compar-real)
  (methods
   (<			<)
   (>			>)
   (<=			<=)
   (>=			>=)
   ;;
   (positive?		positive?)
   (negative?		negative?)
   (non-positive?	non-positive?)
   (non-negative?	non-negative?)
   ;;
   (abs			abs)
   (numerator		numerator)
   (denominator		denominator)
   (sign		sign)
   ;;
   (floor		floor)
   (ceiling		ceiling)
   (truncate		truncate)
   (round		round)
   (rationalize		rationalize)
   #| end of METHODS |# ))

(define-scheme-type <rational-valued>
    <real>
  (constructor #t)
  (type-predicate rational-valued?))

(define-scheme-type <rational>
    <rational-valued>
  (constructor #t)
  (type-predicate  rational?))

;;This "<integer-valued>" is a bit orphan: it is excluded from the hierarchy.
;;
(define-scheme-type <integer-valued>
    <rational-valued>
  (constructor #t)
  (type-predicate integer-valued?))

;;Notice that "<integer>" is a "<rational>", not an "<integer-valued>".
;;
(define-scheme-type <integer>
    <rational>
  (constructor #t)
  (type-predicate integer?)
  (methods
   (gcd			gcd)
   (lcm			lcm)
   (quotient		quotient)
   (remainder		remainder)
   (modulo		modulo)
   (quotient+remainder	quotient+remainder)
   (factorial		factorial)))

(define-scheme-type <exact-integer>
    <integer>
  (constructor #t)
  (type-predicate exact-integer?)
  (hash-function exact-integer-hash)
  (methods
   (div				div)
   (mod				mod)
   (div-and-mod			div-and-mod)
   (div0			div0)
   (mod0			mod0)
   (div0-and-mod0		div0-and-mod0)
   ;;
   (exact-integer-sqrt		exact-integer-sqrt)))

(define-scheme-type <fixnum>
    <exact-integer>
  (constructor #t)
  (type-predicate fixnum?)
  ;;Here we want the equality predicate and comparison function of <exact-integer>.
  (hash-function fixnum-hash))

(define-scheme-type <bignum>
    <exact-integer>
  (constructor #t)
  (type-predicate bignum?)
  ;;Here we want the equality predicate and comparison function of <exact-integer>.
  (hash-function bignum-hash)
  (methods
   (bytevector			bignum->bytevector)
   (odd?			bignum-odd?)
   (even?			bignum-even?)
   #| end of METHODS |# ))

(define-scheme-type <flonum>
    <real>
  (constructor #t)
  (type-predicate flonum?)
  ;;Here we want the equality predicate and comparison function of <real>.
  (hash-function flonum-hash))

(define-scheme-type <ratnum>
    <rational>
  (constructor #t)
  (type-predicate ratnum?)
  ;;Here we want the equality predicate and comparison function of <real>.
  (hash-function ratnum-hash))

(define-scheme-type <compnum>
    <complex>
  (constructor #t)
  (type-predicate compnum?)
  (hash-function compnum-hash))

(define-scheme-type <cflonum>
    <complex>
  (constructor #t)
  (type-predicate cflonum?)
  (hash-function cflonum-hash))

;;; --------------------------------------------------------------------

(define-scheme-type <zero-fixnum>
    <fixnum>
  (constructor #t)
  (type-predicate zero-fixnum?))

(define-scheme-type <positive-fixnum>
    <fixnum>
  (constructor #t)
  (type-predicate positive-fixnum?))

(define-scheme-type <negative-fixnum>
    <fixnum>
  (constructor #t)
  (type-predicate negative-fixnum?))

;;; --------------------------------------------------------------------

(define-scheme-type <positive-bignum>
    <bignum>
  (constructor #t)
  (type-predicate positive-bignum?))

(define-scheme-type <negative-bignum>
    <bignum>
  (constructor #t)
  (type-predicate negative-bignum?))

;;; --------------------------------------------------------------------

(define-scheme-type <positive-ratnum>
    <ratnum>
  (constructor #t)
  (type-predicate positive-ratnum?))

(define-scheme-type <negative-ratnum>
    <ratnum>
  (constructor #t)
  (type-predicate negative-ratnum?))

;;; --------------------------------------------------------------------

(define-scheme-type <zero-flonum>
    <flonum>
  (constructor #t)
  (type-predicate zero-flonum?))

(define-scheme-type <positive-zero-flonum>
    <zero-flonum>
  (constructor #t)
  (type-predicate positive-zero-flonum?))

(define-scheme-type <negative-zero-flonum>
    <zero-flonum>
  (constructor #t)
  (type-predicate negative-zero-flonum?))

;;;

(define-scheme-type <positive-flonum>
    <flonum>
  (constructor #t)
  (type-predicate positive-flonum?))

(define-scheme-type <negative-flonum>
    <flonum>
  (constructor #t)
  (type-predicate negative-flonum?))

;;; --------------------------------------------------------------------

(define-scheme-type <exact-compnum>
    <compnum>
  (constructor #t)
  (type-predicate exact-compnum?))

(define-scheme-type <inexact-compnum>
    <compnum>
  (constructor #t)
  (type-predicate inexact-compnum?))

(define-scheme-type <zero-compnum>
    <inexact-compnum>
  (constructor #t)
  (type-predicate zero-compnum?))

(define-scheme-type <non-zero-inexact-compnum>
    <inexact-compnum>
  (constructor #t)
  (type-predicate non-zero-inexact-compnum?))

;;; --------------------------------------------------------------------

(define-scheme-type <zero-cflonum>
    <cflonum>
  (constructor #t)
  (type-predicate zero-cflonum?))

(define-scheme-type <non-zero-cflonum>
    <cflonum>
  (constructor #t)
  (type-predicate non-zero-cflonum?))


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

   (=				string=?)
   (!=				string!=?)
   (<				string<?)
   (>				string>?)
   (<=				string<=?)
   (>=				string>=?)

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


;;;; vectors

(define-scheme-type <vector>
    <top>
  (constructor vector)
  (type-predicate vector?)
  (equality-predicate vector=?)
  (hash-function vector-hash)
  (methods
   (=				vector=?)
   (!=				vector!=?)
   ;;
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
  (type-predicate empty-vector?))

(define-scheme-type <nevector>
    <vector>
  (constructor <nevector>-constructor)
  (type-predicate nevector?)
  (methods
   (ref				vector-ref)
   (set!			vector-set!)))


;;;; bytevectors

(define-scheme-type <bytevector>
    <top>
  (constructor make-bytevector)
  (type-predicate bytevector?)
  (equality-predicate bytevector=?)
  (hash-function bytevector-hash)
  (methods
   (length		bytevector-length)
   (=			bytevector=?)
   (!=			bytevector!=?)))

(define-scheme-type <empty-bytevector>
    <bytevector>
  (constructor <empty-bytevector>-constructor)
  (type-predicate empty-bytevector?))

(define-scheme-type <nebytevector>
    <bytevector>
  (constructor <nebytevector>-constructor)
  (type-predicate nebytevector?))


;;;; code objects

(define-scheme-type <code>
    <top>
  (constructor #t)
  (type-predicate code?))


;;;; structs

(define-scheme-type <struct>
    <top>
  (constructor #t)
  (type-predicate struct?)
  (equality-predicate struct=?)
  (hash-function struct-hash)
  (methods
   (std				struct-std)
   (name			struct-name)
   (length			struct-length)
   (field-names			struct-field-names)
   (printer			struct-printer)
   (destructor			struct-destructor)
   ;;
   (=				struct=?)
   (!=				struct!=?)
   ;;
   (ref				struct-ref)
   (set!			struct-set!)
   (reset			struct-reset!)
   #| end of METHODS |# ))

(define-scheme-type <struct-type-descriptor>
    <struct>
  (constructor make-struct-type)
  (type-predicate struct-type-descriptor?)
  (methods
   (name			struct-type-name)
   (uid				struct-type-symbol)
   (field-names			struct-type-field-names)
   (constructor			struct-type-constructor)
   (predicate			struct-type-predicate)
   (printer			struct-type-printer)
   (destructor			struct-type-destructor)
   (field-accessor		struct-type-field-accessor)
   (field-mutator		struct-type-field-mutator)
   (field-method		struct-type-field-method)
   #| end of METHODS |# ))


;;;; records

(define-scheme-type <record>
    <struct>
  (constructor #t)
  (type-predicate record?)
  (equality-predicate record=?)
  (hash-function record-hash)
  (methods
   (=				record=?)
   (!=				record!=?)
   ;;
   (ref				record-ref)
   (reset			record-reset!)
   #| end of METHODS |# ))

(define-scheme-type <record-type-descriptor>
    <struct>
  (constructor make-record-type-descriptor)
  (type-predicate record-type-descriptor?)
  (methods
   ;;FIXME Do we want these as methods?  (Marco Maggi; Sat Oct 29, 2016)
   ;;
   ;; (predicate			record-predicate)
   ;; (record-equality-predicate	record-type-equality-predicate)
   ;; (record-comparison-procedure	record-type-comparison-procedure)
   ;; (record-hash-function		record-type-hash-function)
   ;; (destructor			record-type-destructor)
   ;; (printer				record-type-printer)
   ;; (accessor				record-accessor)
   ;; (mutator				record-mutator)
   ;; (field-mutable?			record-field-mutable?)
   ;;
   (name			record-type-name)
   (parent			record-type-parent)
   (uid				record-type-uid)
   (generative?			record-type-generative?)
   (sealed?			record-type-sealed?)
   (opaque?			record-type-opaque?)
   (field-names			record-type-field-names)
   (all-field-names		record-type-all-field-names)
   (uids-list			record-type-uids-list)
   (implemented-interfaces	record-type-implemented-interfaces)
   #| end of METHODS |# ))

(define-scheme-type <record-constructor-descriptor>
    <struct>
  (constructor make-record-constructor-descriptor)
  (type-predicate record-constructor-descriptor?)
  (methods
   ;;FIXME Do we want these as methods?  (Marco Maggi; Sat Oct 29, 2016)
   ;;
   ;; (constructor			record-constructor)
   ;;
   (rtd				rcd-rtd)
   (parent-rcd			rcd-parent-rcd)
   #| end of METHODS |# ))


;;;; hash tables

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

(define-scheme-type <hashtable-equiv>
    <hashtable>
  (constructor make-hashtable)
  (type-predicate hashtable-equiv?))


;;;; condition objects

;;This is the root of all the  condition object types, both simple and compound.  All
;;the simple condition types are derived from "&condition".
;;
(define-scheme-type <condition>
    <record>
  (constructor #t)
  (type-predicate condition?)
  (methods
   (print	print-condition)))

(define-scheme-type <compound-condition>
    <condition>
  (constructor condition)
  (type-predicate compound-condition?))


;;;; unique objects

(define-scheme-type <eof>
    <top>
  (constructor eof-object)
  (type-predicate eof-object?)
  (equality-predicate eq?)
  (hash-function eof-object-hash))

(define-scheme-type <would-block>
    <top>
  (constructor would-block-object)
  (type-predicate would-block-object?)
  (equality-predicate eq?)
  (hash-function would-block-hash))

(define-scheme-type <sentinel>
    <struct>
  (constructor sentinel)
  (type-predicate sentinel?)
  (equality-predicate eq?)
  (hash-function sentinel-hash))


;;;; misc structs and records

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


;;;; input/output ports

(define-scheme-type <port>
    <top>
  (constructor #t)
  (type-predicate port?)
  (equality-predicate eq?)
  (hash-function port-hash)
  (methods
   (open?			open-port?)
   (closed?			closed-port?)
   ;;
   (set-non-blocking-mode	port-set-non-blocking-mode!)
   (unset-non-blocking-mode	port-unset-non-blocking-mode!)
   (non-blocking-mode?		port-in-non-blocking-mode?)
   (reset			<port>-reset)
   ;;
   (has-position?		port-has-port-position?)
   (has-set-position?		port-has-set-port-position!?)
   (position			<port>-position)
   ;;
   (id				port-id)
   (fd				port-fd)
   (uid				port-uid)
   (transcoder			port-transcoder)
   ;;
   (close			close-port)
   (dump-status			port-dump-status)
   ;;
   (putprop			port-putprop)
   (getprop			port-getprop)
   (remprop			port-remprop)
   (property-list		port-property-list)
   #| end of METHODS |# ))

;;; --------------------------------------------------------------------

(define-scheme-type <textual-input-only-port>
    <port>
  (constructor #t)
  (type-predicate textual-input-only-port?)
  (methods
   (mode			<port>-mode)
   (textual-position		port-textual-position)
   ;;
   (eof?			port-eof?)
   #| end of METHODS |# ))

(define-scheme-type <textual-output-only-port>
    <port>
  (constructor #t)
  (type-predicate textual-output-only-port?)
  (methods
   (buffer-mode			<port>-buffer-mode)
   (flush			flush-output-port)
   #| end of METHODS |# ))

(define-scheme-type <textual-input/output-port>
    <port>
  (constructor #t)
  (type-predicate textual-input/output-port?)
  (methods
   (mode			<port>-mode)
   (buffer-mode			<port>-buffer-mode)
   (textual-position		port-textual-position)
   ;;
   (eof?			port-eof?)
   (flush			flush-output-port)
   #| end of METHODS |# ))

;;; --------------------------------------------------------------------

(define-scheme-type <binary-input-only-port>
    <port>
  (constructor #t)
  (type-predicate binary-input-only-port?)
  (methods
   (eof?			port-eof?)
   #| end of METHODS |# ))

(define-scheme-type <binary-output-only-port>
    <port>
  (constructor #t)
  (type-predicate binary-output-only-port?)
  (methods
   (flush			flush-output-port)
   #| end of METHODS |# ))

(define-scheme-type <binary-input/output-port>
    <port>
  (constructor #t)
  (type-predicate binary-input/output-port?)
  (methods
   (eof?			port-eof?)
   (flush			flush-output-port)
   #| end of METHODS |# ))


;;;; list types

(define-scheme-type <list>
    <top>
  (constructor list)
  (type-predicate list?)
  (equality-predicate equal?)
  (hash-function list-hash)
  (methods
   (circular?		circular-list?)
   (single-item?	list-of-single-item?)
   ;;
   (length		length)
   ;;
   (append		append)
   (reverse		reverse)
   (tail		list-tail)
   (ref			list-ref)
   #| end of METHODS |# ))

(define-scheme-type <nelist>
    <list>
  (constructor <nelist>-constructor)
  (type-predicate <nelist>-type-predicate)
  (methods
   (car			car)
   (cdr			cdr)
   (last-pair		last-pair)))

(define-scheme-type <null>
    <list>
  (constructor <null>-constructor)
  (type-predicate null?))

;;; --------------------------------------------------------------------

(define-scheme-type <pair>
    <top>
  (constructor cons)
  (type-predicate pair?)
  (equality-predicate equal?)
  (hash-function pair-hash)
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
  (hash-function ipair-hash)
  (methods
   (car		icar)
   (cdr		icdr)))


;;;; done

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
