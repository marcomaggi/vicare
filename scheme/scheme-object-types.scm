;; scheme-object-types.scm --
;;
;; Definitions of built-in Scheme object types.

(define-scheme-type <top>
    #f
  <top>-constructor <top>-type-predicate
  (methods
   (hash		object-hash)))


;;;; standalone object types

(define-scheme-type <void>
    <top>
  void void-object?)

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
  #f number?)

(define-scheme-type <complex>
    <number>
  make-rectangular complex?)

(define-scheme-type <real-valued>
    <complex>
  #f real-valued?)

(define-scheme-type <real>
    <real-valued>
  #f real?)

(define-scheme-type <rational-valued>
    <real>
  #f rational-valued?)

(define-scheme-type <rational>
    <rational-valued>
  #f rational?)

;;This "<integer-valued>" is a bit orphan: it is excluded from the hierarchy.
;;
(define-scheme-type <integer-valued>
    <rational-valued>
  #f integer-valued?)

;;Notice that "<integer>" is a "<rational>", not an "<integer-valued>".
;;
(define-scheme-type <integer>
    <rational>
  #f integer?)

(define-scheme-type <exact-integer>
    <integer>
  #f exact-integer?)

(define-scheme-type <fixnum>
    <exact-integer>
  #f fixnum?)

(define-scheme-type <flonum>
    <real-valued>
  #f flonum?)

(define-scheme-type <ratnum>
    <rational>
  #f ratnum?)

(define-scheme-type <bignum>
    <exact-integer>
  #f bignum?)

(define-scheme-type <compnum>
    <complex>
  #f compnum?)

(define-scheme-type <cflonum>
    <complex>
  #f cflonum?)


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
  vector vector?)

(define-scheme-type <pair>
    <top>
  cons pair?
  (methods
   (car		car)
   (cdr		cdr)))

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

(define-scheme-type <condition>
    <record>
  #f condition?
  (methods
   (print	print-condition)))

(define-scheme-type <compound-condition>
    <condition>
  condition compound-condition?)


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

(define-scheme-type <nlist>
    <list>
  nlist nlist?
  (methods
   (car		car)
   (cdr		cdr)))

(define-scheme-type <null>
    <list>
  <null>-constructor null?)


;;;; done

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'define-scheme-type	'scheme-indent-function 2)
;; eval: (put 'define-list-type		'scheme-indent-function 2)
;; End:
