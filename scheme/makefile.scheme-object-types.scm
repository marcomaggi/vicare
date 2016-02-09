;; scheme-object-types.scm --
;;
;; Definitions of built-in Scheme object types.


;;;; core syntactic binding descriptors: built-in Scheme object types

(define-scheme-type <top>
    #f
  <top>-constructor <top>-type-predicate
  (methods
   (hash		object-hash)))

(define-scheme-type <no-return>
    #f
  #f always-false)


;;;; standalone object types

(define-scheme-type <void>
    <top>
  void void-object?)

(define-scheme-type <eof>
    <top>
  eof-object eof-object?)

(define-scheme-type <would-block>
    <top>
  would-block-object would-block-object?)

(define-scheme-type <boolean>
    <top>
  <boolean>-constructor boolean?)

(define-scheme-type <true>
    <boolean>
  #f #f)

(define-scheme-type <false>
    <boolean>
  #f #f)

(define-scheme-type <char>
    <top>
  integer->char char?
  (methods
   (string		string)
   (hash		char-hash)
   (integer		char->integer)
   (fixnum		char->fixnum)))

(define-scheme-type <symbol>
    <top>
  string->symbol symbol?
  (methods
   (string		symbol->string)
   (hash		symbol-hash)
   (bound?		symbol-bound?)
   (value		<symbol>-value)
   (putprop		putprop)
   (getprop		getprop)
   (remprop		remprop)
   (property-list	property-list)))

(define-scheme-type <keyword>
    <top>
  symbol->keyword keyword?
  (methods
   (symbol		keyword->symbol)
   (string		keyword->string)
   (hash		keyword-hash)))

(define-scheme-type <pointer>
    <top>
  integer->pointer pointer?
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
   (set-null!		set-pointer-null!)
   (hash		pointer-hash)))

(define-scheme-type <transcoder>
    <top>
  make-transcoder transcoder?
  (methods
   (codec		transcoder-codec)
   (eol-style		transcoder-eol-style)
   (handling-mode	transcoder-error-handling-mode)))


;;;; procedures

(define-scheme-type <procedure>
    <top>
  #f procedure?)

(define-scheme-type <predicate>
    <procedure>
  #f #f)


;;; numeric types

(define-scheme-type <number>
    <top>
  #t number?)

(define-scheme-type <complex>
    <number>
  make-rectangular complex?)

(define-scheme-type <real-valued>
    <complex>
  #t real-valued?)

(define-scheme-type <real>
    <real-valued>
  #t real?)

(define-scheme-type <rational-valued>
    <real>
  #t rational-valued?)

(define-scheme-type <rational>
    <rational-valued>
  #t rational?)

;;This "<integer-valued>" is a bit orphan: it is excluded from the hierarchy.
;;
(define-scheme-type <integer-valued>
    <rational-valued>
  #t integer-valued?)

;;Notice that "<integer>" is a "<rational>", not an "<integer-valued>".
;;
(define-scheme-type <integer>
    <rational>
  #t integer?)

(define-scheme-type <exact-integer>
    <integer>
  #t exact-integer?)

(define-scheme-type <fixnum>
    <exact-integer>
  #t fixnum?)

(define-scheme-type <flonum>
    <real>
  #t flonum?)

(define-scheme-type <ratnum>
    <rational>
  #t ratnum?)

(define-scheme-type <bignum>
    <exact-integer>
  #t bignum?)

(define-scheme-type <compnum>
    <complex>
  #t compnum?)

(define-scheme-type <cflonum>
    <complex>
  #t cflonum?)

;;; --------------------------------------------------------------------

(define-scheme-type <non-negative-fixnum>
    <fixnum>
  #t non-negative-fixnum?)

(define-scheme-type <zero-fixnum>
    <non-negative-fixnum>
  #t zero-fixnum?)

(define-scheme-type <positive-fixnum>
    <non-negative-fixnum>
  #t positive-fixnum?)

(define-scheme-type <negative-fixnum>
    <fixnum>
  #t negative-fixnum?)

;;; --------------------------------------------------------------------

(define-scheme-type <positive-bignum>
    <bignum>
  #t bignum-positive?)

(define-scheme-type <negative-bignum>
    <bignum>
  #t bignum-negative?)

;;FIXME When type unions are implemented we should uncomment this.  (Marco Maggi; Sun
;;Dec 27, 2015)
;;
;; (define-union-type <non-negative-exact-integer>
;;     non-negative-exact-integer?
;;   (or <non-negative-fixnum>
;;       <positive-bignum>))
;;

;;; --------------------------------------------------------------------

;; (define-scheme-type <file-descriptor>
;;     <non-negative-fixnum>
;;   #f #f)

;;; --------------------------------------------------------------------

(define-scheme-type <non-negative-flonum>
    <flonum>
  #t non-negative-flonum?)

(define-scheme-type <positive-flonum>
    <non-negative-flonum>
  #t positive-flonum?)

(define-scheme-type <zero-flonum>
    <non-negative-flonum>
  #t zero-flonum?)

(define-scheme-type <negative-flonum>
    <flonum>
  #t negative-flonum?)

(define-scheme-type <positive-zero-flonum>
    <zero-flonum>
  #t positive-zero-flonum?)

(define-scheme-type <negative-zero-flonum>
    <zero-flonum>
  #t negative-zero-flonum?)

;;; --------------------------------------------------------------------

(define-scheme-type <exact-compnum>
    <compnum>
  #t exact-compnum?)


;;;; compound types

(define-scheme-type <string>
    <top>
  string string?
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

   (hash			string-hash)
   (symbol			string->symbol)
   (keyword			string->keyword)
   (list			string->list)
   ))

(define-scheme-type <vector>
    <top>
  vector vector?
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

(define-scheme-type <bytevector>
    <top>
  make-bytevector bytevector?)

;;; --------------------------------------------------------------------

(define-scheme-type <hashtable>
    <top>
  #f hashtable?)

(define-scheme-type <hashtable-eq>
    <hashtable>
  make-eq-hashtable hashtable-eq?)

(define-scheme-type <hashtable-eqv>
    <hashtable>
  make-eqv-hashtable hashtable-eqv?)

(define-scheme-type <hashtable-equal>
    <hashtable>
  make-hashtable hashtable-equiv?)

;;; --------------------------------------------------------------------

(define-scheme-type <code>
    <top>
  #f code?)


;;;; records and structs

(define-scheme-type <struct>
    <top>
  #f struct?)

(define-scheme-type <struct-type-descriptor>
    <struct>
  make-struct-type struct-type-descriptor?)

;;; --------------------------------------------------------------------

(define-scheme-type <record>
    <struct>
  #f record?)

(define-scheme-type <record-type-descriptor>
    <struct>
  make-record-type-descriptor record-type-descriptor?)

(define-scheme-type <record-constructor-descriptor>
    <struct>
  make-record-constructor-descriptor record-constructor-descriptor?)

;;; --------------------------------------------------------------------

(define-scheme-type <opaque-record>
    <top>
  #f always-false)

;;; --------------------------------------------------------------------

;;This is the root of all the  condition object types, both simple and compound.  All
;;the simple condition types are derived from "&condition".
;;
(define-scheme-type <condition>
    <record>
  #f condition?
  (methods
   (print	print-condition)))

(define-scheme-type <compound-condition>
    <condition>
  condition compound-condition?)

;;; --------------------------------------------------------------------

(define-scheme-type <promise>
    <record>
  make-promise promise?
  (methods
   (force	force)))

(define-scheme-type <enum-set>
    <struct>
  make-enumeration enum-set?
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
  uname utsname?
  (methods
   (sysname		utsname-sysname)
   (nodename		utsname-nodename)
   (release		utsname-release)
   (version		utsname-version)
   (machine		utsname-machine)))

;;; --------------------------------------------------------------------

(define-scheme-type <memory-block>
    <struct>
  make-memory-block memory-block?
  (methods
   (pointer		memory-block-pointer)
   (size		memory-block-size)
   (reset		memory-block-reset)))

;;; --------------------------------------------------------------------

(define-scheme-type <stats>
    <opaque-record>
  #f stats?
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


;;;; input/output ports

(define-scheme-type <port>
    <top>
  #f port?)

;;; --------------------------------------------------------------------

(define-scheme-type <input-port>
    <port>
  #f input-port?)

(define-scheme-type <output-port>
    <port>
  #f output-port?)

(define-scheme-type <input/output-port>
    <port>
  #f input/output-port?)

;;; --------------------------------------------------------------------

(define-scheme-type <textual-port>
    <port>
  #f textual-port?)

(define-scheme-type <binary-port>
    <port>
  #f binary-port?)

;;; --------------------------------------------------------------------

(define-scheme-type <textual-input-port>
    <input-port>
  #f textual-input-port?)

(define-scheme-type <textual-output-port>
    <output-port>
  #f textual-output-port?)

(define-scheme-type <textual-input/output-port>
    <input/output-port>
  #f textual-input/output-port?)

;;; --------------------------------------------------------------------

(define-scheme-type <binary-input-port>
    <input-port>
  #f binary-input-port?)

(define-scheme-type <binary-output-port>
    <output-port>
  #f binary-output-port?)

(define-scheme-type <binary-input/output-port>
    <input/output-port>
  #f binary-input/output-port?)


;;;; list types

(define-scheme-type <list>
    <top>
  list list?
  (methods
   (car		car)
   (cdr		cdr)))

(define-scheme-type <null>
    <list>
  <null>-constructor null?)

(define-scheme-type <pair>
    <list>
  cons pair?
  (methods
   (car		car)
   (cdr		cdr)))

(define-scheme-type <nlist>
    <pair>
  nlist nlist?
  (methods
   (car		car)
   (cdr		cdr)))

(define-scheme-type <standalone-pair>
    <pair>
  #f standalone-pair?)


;;;; miscellaneous compound types

;;FIXME When type unions are implemented we should uncomment this.  (Marco Maggi; Sun
;;Dec 27, 2015)
;;
;; (define-union-type <syntax-object>
;;     syntax-object?
;;   (or <syntactic-identifier>
;;       <boolean>
;;       <char>
;;       <number>
;;       <string>
;;       <bytevector>
;;       <vector-of-syntax-objects>
;;       <list-of-syntax-objects>
;;       ))


;;;; done

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'define-scheme-type	'scheme-indent-function 2)
;; eval: (put 'define-union-type	'scheme-indent-function 2)
;; eval: (put 'define-list-type		'scheme-indent-function 2)
;; End:
