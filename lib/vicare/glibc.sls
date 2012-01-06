;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: GNU C Library platform API
;;;Date: Wed Nov  9, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


(library (vicare glibc)
  (export
    ;; operating system environment variables
    clearenv

    ;; file system directories
    dirfd

    ;; temporary files and directories
    mkstemp			mkdtemp

    ;; file system synchronisation
    sync			fsync
    fdatasync

    ;; sockets
    if-nametoindex		if-indextoname
    if-nameindex

    ;; mathematics
    csin		ccos		ctan
    casin		cacos		catan
    cexp		clog		clog10
    csqrt		cpow
    glibc-sinh		glibc-cosh	glibc-tanh
    glibc-asinh		glibc-acosh	glibc-atanh
    csinh		ccosh		ctanh
    casinh		cacosh		catanh
    erf			erfc		tgamma		lgamma
    j0			j1		y0
    y1			jn		yn

    ;; random numbers
    rand		srand

    ;; pattern matching, globbing, regular expressions
    fnmatch		glob		glob/string
    regcomp		regexec		regfree

    ;; word expansion
    wordexp		wordexp/string

    ;; system configuration
    sysconf
    pathconf		fpathconf
    confstr		confstr/string

    ;; generic character set conversion
    iconv-open		iconv?
    iconv-close		iconv-closed?
    iconv!
    enum-iconv-encoding		iconv-encoding
    iconv-encoding-universe
    iconv-encoding-aliases?	iconv-encoding=?
    )
  (import (vicare)
    (prefix (only (vicare posix)
		  directory-stream?
		  directory-stream-closed?
		  directory-stream-pointer)
	    posix.)
    (prefix (only (ikarus system $foreign)
		  pointer?
		  pointer-null?)
	    ffi.)
    (vicare syntactic-extensions)
    (vicare platform-constants)
    (prefix (vicare unsafe-capi)
	    capi.)
    (prefix (vicare words)
	    words.)
    (prefix (vicare unsafe-operations)
	    unsafe.))


;;;; helpers

(define (raise-errno-error who errno . irritants)
  (raise (condition
	  (make-error)
	  (make-errno-condition errno)
	  (make-who-condition who)
	  (make-message-condition (strerror errno))
	  (make-irritants-condition irritants))))


;;;; arguments validation

(define-argument-validation (procedure who obj)
  (procedure? obj)
  (assertion-violation who "expected procedure as argument" obj))

(define-argument-validation (boolean who obj)
  (boolean? obj)
  (assertion-violation who "expected boolean as argument" obj))

(define-argument-validation (fixnum who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum as argument" obj))

(define-argument-validation (string who obj)
  (string? obj)
  (assertion-violation who "expected string as argument" obj))

(define-argument-validation (symbol who obj)
  (symbol? obj)
  (assertion-violation who "expected symbol as argument" obj))

(define-argument-validation (bytevector who obj)
  (bytevector? obj)
  (assertion-violation who "expected bytevector as argument" obj))

(define-argument-validation (flonum who obj)
  (flonum? obj)
  (assertion-violation who "expected flonum as argument" obj))

(define-argument-validation (cflonum who obj)
  (cflonum? obj)
  (assertion-violation who "expected complex flonum as argument" obj))

(define-argument-validation (pointer who obj)
  (ffi.pointer? obj)
  (assertion-violation who "expected pointer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (string/bytevector who obj)
  (or (string? obj) (bytevector? obj))
  (assertion-violation who "expected string or bytevector as argument" obj))

(define-argument-validation (false/procedure who obj)
  (or (not obj) (procedure? obj))
  (assertion-violation who "expected false or procedure as argument" obj))

(define-argument-validation (index who obj)
  (and (fixnum? obj) (unsafe.fx<= 0 obj))
  (assertion-violation who "expected non-negative fixnum as argument" obj))

(define-argument-validation (false/index who obj)
  (or (not obj) (and (fixnum? obj) (unsafe.fx<= 0 obj)))
  (assertion-violation who "expected false or non-negative fixnum as argument" obj))

(define-argument-validation (iconv who obj)
  (iconv? obj)
  (assertion-violation who "expected iconv handle as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (pid who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum pid as argument" obj))

(define-argument-validation (signal who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum signal code as argument" obj))

(define-argument-validation (directory-stream who obj)
  (posix.directory-stream? obj)
  (assertion-violation who "expected directory stream as argument" obj))

(define-argument-validation (open-directory-stream who obj)
  (not (posix.directory-stream-closed? obj))
  (assertion-violation who "expected open directory stream as argument" obj))

(define-argument-validation (file-descriptor who obj)
  (and (fixnum? obj) (unsafe.fx<= 0 obj))
  (assertion-violation who "expected fixnum file descriptor as argument" obj))

(define-argument-validation (iconv-set who obj)
  (enum-set-subset? obj iconv-encoding-universe)
  (assertion-violation who "expected enum set in iconv encoding universe" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (unsigned-int who obj)
  (words.unsigned-int? obj)
  (assertion-violation who
    "expected exact integer representing a C language \"unsigned int\" as argument" obj))

(define-argument-validation (signed-int who obj)
  (words.signed-int? obj)
  (assertion-violation who
    "expected exact integer representing a C language \"signed int\" as argument" obj))


;;;; operating system environment variables

(define (clearenv)
  (capi.glibc-clearenv))


;;;; file system directories

(define (dirfd stream)
  (define who 'dirfd)
  (with-arguments-validation (who)
      ((directory-stream       stream)
       (open-directory-stream  stream))
    (let ((rv (capi.glibc-dirfd (posix.directory-stream-pointer stream))))
      (if (unsafe.fx<= 0 rv)
	  rv
	(raise-errno-error who rv stream)))))


;;;; temporary files and directories

(define (mkstemp template)
  (define who 'mkstemp)
  (with-arguments-validation (who)
      ((bytevector  template))
    (let ((rv (capi.glibc-mkstemp template)))
      (if (unsafe.fx<= 0 rv)
	  rv
	(raise-errno-error who rv template)))))

(define (mkdtemp template)
  (define who 'mkdtemp)
  (with-arguments-validation (who)
      ((bytevector  template))
    (let ((rv (capi.glibc-mkdtemp template)))
      (if (fixnum? rv)
	  (raise-errno-error who rv template)
	rv))))


;;;; file system synchronisation

(define (sync)
  (define who 'sync)
  (let ((rv (capi.glibc-sync)))
    (unless (unsafe.fxzero? rv)
      (raise-errno-error who rv))))

(define (fsync fd)
  (define who 'fsync)
  (with-arguments-validation (who)
      ((file-descriptor  fd))
    (let ((rv (capi.glibc-fsync fd)))
      (unless (unsafe.fxzero? rv)
	(raise-errno-error who rv fd)))))

(define (fdatasync fd)
  (define who 'fdatasync)
  (with-arguments-validation (who)
      ((file-descriptor  fd))
    (let ((rv (capi.glibc-fdatasync fd)))
      (unless (unsafe.fxzero? rv)
	(raise-errno-error who rv fd)))))


;;;; sockets

(define (if-nametoindex name)
  (define who 'if-nametoindex)
  (with-arguments-validation (who)
      ((string	name))
    (capi.glibc-if-nametoindex (string->utf8 name))))

(define (if-indextoname index)
  (define who 'if-indextoname)
  (with-arguments-validation (who)
      ((fixnum	index))
    (let ((rv (capi.glibc-if-indextoname index)))
      (and rv (utf8->string rv)))))

(define (if-nameindex)
  (let ((rv (capi.glibc-if-nameindex)))
    (map (lambda (entry)
	   (cons (car entry) (utf8->string (cdr entry))))
      rv)))


;;;; mathematics

(define-syntax define-one-operand/flonum
  (syntax-rules ()
    ((_ ?who ?func)
     (define (?who X)
       (define who '?who)
       (with-arguments-validation (who)
	   ((flonum	X))
	 (?func X))))))

(define-syntax define-two-operands/flonum
  (syntax-rules ()
    ((_ ?who ?func)
     (define (?who X Y)
       (define who '?who)
       (with-arguments-validation (who)
	   ((flonum	X)
	    (flonum	Y))
	 (?func X Y))))))

;;; --------------------------------------------------------------------

(define-syntax define-one-operand/cflonum
  (syntax-rules ()
    ((_ ?who ?func)
     (define (?who X)
       (define who '?who)
       (with-arguments-validation (who)
	   ((cflonum	X))
	 (?func X))))))

(define-syntax define-two-operands/cflonum
  (syntax-rules ()
    ((_ ?who ?func)
     (define (?who X Y)
       (define who '?who)
       (with-arguments-validation (who)
	   ((cflonum	X)
	    (cflonum	Y))
	 (?func X Y))))))

;;; --------------------------------------------------------------------

(define-one-operand/cflonum csin	capi.glibc-csin)
(define-one-operand/cflonum ccos	capi.glibc-ccos)
(define-one-operand/cflonum ctan	capi.glibc-ctan)
(define-one-operand/cflonum casin	capi.glibc-casin)
(define-one-operand/cflonum cacos	capi.glibc-cacos)
(define-one-operand/cflonum catan	capi.glibc-catan)

(define-one-operand/cflonum cexp	capi.glibc-cexp)
(define-one-operand/cflonum clog	capi.glibc-clog)
(define-one-operand/cflonum clog10	capi.glibc-clog10)
(define-one-operand/cflonum csqrt	capi.glibc-csqrt)
(define-two-operands/cflonum cpow	capi.glibc-cpow)

(define-one-operand/flonum glibc-sinh	capi.glibc-sinh)
(define-one-operand/flonum glibc-cosh	capi.glibc-cosh)
(define-one-operand/flonum glibc-tanh	capi.glibc-tanh)
(define-one-operand/flonum glibc-asinh	capi.glibc-asinh)
(define-one-operand/flonum glibc-acosh	capi.glibc-acosh)
(define-one-operand/flonum glibc-atanh	capi.glibc-atanh)
(define-one-operand/cflonum csinh	capi.glibc-csinh)
(define-one-operand/cflonum ccosh	capi.glibc-ccosh)
(define-one-operand/cflonum ctanh	capi.glibc-ctanh)
(define-one-operand/cflonum casinh	capi.glibc-casinh)
(define-one-operand/cflonum cacosh	capi.glibc-cacosh)
(define-one-operand/cflonum catanh	capi.glibc-catanh)

;;; --------------------------------------------------------------------

(define-one-operand/flonum erf		capi.glibc-erf)
(define-one-operand/flonum erfc		capi.glibc-erfc)
(define-one-operand/flonum tgamma	capi.glibc-tgamma)
(define-one-operand/flonum j0		capi.glibc-j0)
(define-one-operand/flonum j1		capi.glibc-j1)
(define-one-operand/flonum y0		capi.glibc-y0)
(define-one-operand/flonum y1		capi.glibc-y1)

(define (lgamma X)
  (define who 'lgamma)
  (with-arguments-validation (who)
      ((flonum	X))
    (let ((rv (capi.glibc-lgamma X)))
      (values (car rv) (cdr rv)))))

(define (jn N X)
  (define who 'jn)
  (with-arguments-validation (who)
      ((flonum	X)
       (fixnum	N))
    (capi.glibc-jn N X)))

(define (yn N X)
  (define who 'yn)
  (with-arguments-validation (who)
      ((flonum	X)
       (fixnum	N))
    (capi.glibc-yn N X)))


;;;; random numbers

(define (rand)
  (capi.glibc-rand))

(define (srand seed)
  (with-arguments-validation (srand)
      ((unsigned-int	seed))
    (capi.glibc-srand seed)))


;;;; pattern matching, globbing, regular expressions

(define (fnmatch pattern string flags)
  (define who 'fnmatch)
  (with-arguments-validation (who)
      ((string/bytevector	pattern)
       (string/bytevector	string)
       (fixnum			flags))
    (with-bytevectors ((pattern.bv	pattern)
		       (string.bv	string))
      (capi.glibc-fnmatch pattern.bv string.bv flags))))

(define (glob pattern flags error-handler)
  (define who 'glob)
  (with-arguments-validation (who)
      ((string/bytevector	pattern)
       (fixnum			flags)
       (false/procedure		error-handler))
    (with-bytevectors ((pattern.bv	pattern))
      (capi.glibc-glob pattern.bv flags error-handler))))

(define (glob/string pattern flags error-handler)
  (let ((rv (glob pattern flags error-handler)))
    (if (fixnum? rv)
	rv
      (map latin1->string rv))))

;;; --------------------------------------------------------------------

(define %regex-guardian
  (make-guardian))

(define (%free-allocated-regex)
  (do ((bv (%regex-guardian) (%regex-guardian)))
      ((not bv))
    (capi.glibc-regfree bv)))

(define (regcomp pattern flags)
  (define who 'regcomp)
  (with-arguments-validation (who)
      ((string/bytevector	pattern)
       (fixnum			flags))
    (with-bytevectors ((pattern.bv pattern))
      (let ((rv (capi.glibc-regcomp pattern.bv flags)))
	(if (bytevector? rv)
	    (%regex-guardian rv)
	  (error who (latin1->string (cdr rv)) (car rv) pattern flags))))))

(define (regexec regex string flags)
  (define who 'regexec)
  (with-arguments-validation (who)
      ((bytevector		regex)
       (string/bytevector	string)
       (fixnum			flags))
    (with-bytevectors ((string.bv string))
      (let ((rv (capi.glibc-regexec regex string.bv flags)))
	(if (or (not rv) (vector? rv))
	    rv
	  (error who (latin1->string (cdr rv)) (car rv) regex string flags))))))

(define (regfree regex)
  (define who 'regfree)
  (with-arguments-validation (who)
      ((bytevector  regex))
    (capi.glibc-regfree regex)))


;;;; word expansion

(define (wordexp words flags)
  (define who 'wordexp)
  (with-arguments-validation (who)
      ((string/bytevector	words)
       (fixnum			flags))
    (with-bytevectors ((words.bv words))
      (capi.glibc-wordexp words.bv flags))))

(define (wordexp/string words flags)
  (let ((rv (wordexp words flags)))
    (if (vector? rv)
	(vector-map latin1->string rv)
      rv)))


;;;; system configuration

(define (sysconf parameter)
  (define who 'sysconf)
  (with-arguments-validation (who)
      ((signed-int	parameter))
    (let ((rv (capi.glibc-sysconf parameter)))
      (if rv
	  (if (negative? rv)
	      (raise-errno-error who rv parameter)
	    rv)
	rv))))

;;; --------------------------------------------------------------------

(define (pathconf pathname parameter)
  (define who 'pathconf)
  (with-arguments-validation (who)
      ((string/bytevector	pathname)
       (signed-int		parameter))
    (with-pathnames ((pathname.bv pathname))
      (let ((rv (capi.glibc-pathconf pathname.bv parameter)))
	(if rv
	    (if (negative? rv)
		(raise-errno-error who rv pathname parameter)
	      rv)
	  rv)))))

(define (fpathconf fd parameter)
  (define who 'fpathconf)
  (with-arguments-validation (who)
      ((file-descriptor	fd)
       (signed-int	parameter))
    (let ((rv (capi.glibc-fpathconf fd parameter)))
      (if rv
	  (if (negative? rv)
	      (raise-errno-error who rv fd parameter)
	    rv)
	rv))))

;;; --------------------------------------------------------------------

(define (confstr parameter)
  (define who 'confstr)
  (with-arguments-validation (who)
      ((signed-int	parameter))
    (let ((rv (capi.glibc-confstr parameter)))
      (if (bytevector? rv)
	  rv
	(raise-errno-error who rv parameter)))))

(define (confstr/string parameter)
  (latin1->string (confstr parameter)))


;;;; general character set conversion

(define-enumeration enum-iconv-encoding
  ( ;;
   TRANSLIT IGNORE

   ANSI_X3.4-1968 ANSI_X3.4-1986 ASCII CP367 IBM367 ISO-IR-6 ISO646-US ISO_646.IRV:1991 US US-ASCII CSASCII
   UTF-8
   ISO-10646-UCS-2 UCS-2 CSUNICODE
   UCS-2BE UNICODE-1-1 UNICODEBIG CSUNICODE11
   UCS-2LE UNICODELITTLE
   ISO-10646-UCS-4 UCS-4 CSUCS4
   UCS-4BE
   UCS-4LE
   UTF-16
   UTF-16BE
   UTF-16LE
   UTF-32
   UTF-32BE
   UTF-32LE
   UNICODE-1-1-UTF-7 UTF-7 CSUNICODE11UTF7
   UCS-2-INTERNAL
   UCS-2-SWAPPED
   UCS-4-INTERNAL
   UCS-4-SWAPPED
   C99
   JAVA
   CP819 IBM819 ISO-8859-1 ISO-IR-100 ISO8859-1 ISO_8859-1 ISO_8859-1:1987 L1 LATIN1 CSISOLATIN1
   ISO-8859-2 ISO-IR-101 ISO8859-2 ISO_8859-2 ISO_8859-2:1987 L2 LATIN2 CSISOLATIN2
   ISO-8859-3 ISO-IR-109 ISO8859-3 ISO_8859-3 ISO_8859-3:1988 L3 LATIN3 CSISOLATIN3
   ISO-8859-4 ISO-IR-110 ISO8859-4 ISO_8859-4 ISO_8859-4:1988 L4 LATIN4 CSISOLATIN4
   CYRILLIC ISO-8859-5 ISO-IR-144 ISO8859-5 ISO_8859-5 ISO_8859-5:1988 CSISOLATINCYRILLIC
   ARABIC ASMO-708 ECMA-114 ISO-8859-6 ISO-IR-127 ISO8859-6 ISO_8859-6 ISO_8859-6:1987 CSISOLATINARABIC
   ECMA-118 ELOT_928 GREEK GREEK8 ISO-8859-7 ISO-IR-126 ISO8859-7 ISO_8859-7 ISO_8859-7:1987 ISO_8859-7:2003 CSISOLATINGREEK
   HEBREW ISO-8859-8 ISO-IR-138 ISO8859-8 ISO_8859-8 ISO_8859-8:1988 CSISOLATINHEBREW
   ISO-8859-9 ISO-IR-148 ISO8859-9 ISO_8859-9 ISO_8859-9:1989 L5 LATIN5 CSISOLATIN5
   ISO-8859-10 ISO-IR-157 ISO8859-10 ISO_8859-10 ISO_8859-10:1992 L6 LATIN6 CSISOLATIN6
   ISO-8859-11 ISO8859-11 ISO_8859-11
   ISO-8859-13 ISO-IR-179 ISO8859-13 ISO_8859-13 L7 LATIN7
   ISO-8859-14 ISO-CELTIC ISO-IR-199 ISO8859-14 ISO_8859-14 ISO_8859-14:1998 L8 LATIN8
   ISO-8859-15 ISO-IR-203 ISO8859-15 ISO_8859-15 ISO_8859-15:1998 LATIN-9
   ISO-8859-16 ISO-IR-226 ISO8859-16 ISO_8859-16 ISO_8859-16:2001 L10 LATIN10
   KOI8-R CSKOI8R
   KOI8-U
   KOI8-RU
   CP1250 MS-EE WINDOWS-1250
   CP1251 MS-CYRL WINDOWS-1251
   CP1252 MS-ANSI WINDOWS-1252
   CP1253 MS-GREEK WINDOWS-1253
   CP1254 MS-TURK WINDOWS-1254
   CP1255 MS-HEBR WINDOWS-1255
   CP1256 MS-ARAB WINDOWS-1256
   CP1257 WINBALTRIM WINDOWS-1257
   CP1258 WINDOWS-1258
;;;Commented out because they are numbers, not symbols:
;;;
;;; 850 862 866
;;;
;;;this is not a problem because  CP850, CP862 and CP866 are aliases for
;;;them.
;;;
   CP850 IBM850 CSPC850MULTILINGUAL
   CP862 IBM862 CSPC862LATINHEBREW
   CP866 IBM866 CSIBM866
   CP1131
   MAC MACINTOSH MACROMAN CSMACINTOSH
   MACCENTRALEUROPE
   MACICELAND
   MACCROATIAN
   MACROMANIA
   MACCYRILLIC
   MACUKRAINE
   MACGREEK
   MACTURKISH
   MACHEBREW
   MACARABIC
   MACTHAI
   HP-ROMAN8 R8 ROMAN8 CSHPROMAN8
   NEXTSTEP
   ARMSCII-8
   GEORGIAN-ACADEMY
   GEORGIAN-PS
   KOI8-T
   CP154 CYRILLIC-ASIAN PT154 PTCP154 CSPTCP154
   KZ-1048 RK1048 STRK1048-2002 CSKZ1048
   MULELAO-1
   CP1133 IBM-CP1133
   ISO-IR-166 TIS-620 TIS620 TIS620-0 TIS620.2529-1 TIS620.2533-0 TIS620.2533-1
   CP874 WINDOWS-874
   VISCII VISCII1.1-1 CSVISCII
   TCVN TCVN-5712 TCVN5712-1 TCVN5712-1:1993
   ISO-IR-14 ISO646-JP JIS_C6220-1969-RO JP CSISO14JISC6220RO
   JISX0201-1976 JIS_X0201 X0201 CSHALFWIDTHKATAKANA
   ISO-IR-87 JIS0208 JIS_C6226-1983 JIS_X0208 JIS_X0208-1983 JIS_X0208-1990 X0208 CSISO87JISX0208
   ISO-IR-159 JIS_X0212 JIS_X0212-1990 JIS_X0212.1990-0 X0212 CSISO159JISX02121990
   CN GB_1988-80 ISO-IR-57 ISO646-CN CSISO57GB1988
   CHINESE GB_2312-80 ISO-IR-58 CSISO58GB231280
   CN-GB-ISOIR165 ISO-IR-165
   ISO-IR-149 KOREAN KSC_5601 KS_C_5601-1987 KS_C_5601-1989 CSKSC56011987
   EUC-JP EUCJP EXTENDED_UNIX_CODE_PACKED_FORMAT_FOR_JAPANESE CSEUCPKDFMTJAPANESE
   MS_KANJI SHIFT-JIS SHIFT_JIS SJIS CSSHIFTJIS
   CP932
   ISO-2022-JP CSISO2022JP
   ISO-2022-JP-1
   ISO-2022-JP-2 CSISO2022JP2
   CN-GB EUC-CN EUCCN GB2312 CSGB2312
   GBK
   CP936 MS936 WINDOWS-936
   GB18030
   ISO-2022-CN CSISO2022CN
   ISO-2022-CN-EXT
   HZ HZ-GB-2312
   EUC-TW EUCTW CSEUCTW
   BIG-5 BIG-FIVE BIG5 BIGFIVE CN-BIG5 CSBIG5
   CP950
   BIG5-HKSCS:1999
   BIG5-HKSCS:2001
   BIG5-HKSCS BIG5-HKSCS:2004 BIG5HKSCS
   EUC-KR EUCKR CSEUCKR
   CP949 UHC
   CP1361 JOHAB
   ISO-2022-KR CSISO2022KR)
  iconv-encoding)

(define iconv-encoding-universe
  (enum-set-universe (iconv-encoding)))

;;; --------------------------------------------------------------------

(define (%set->encoding-symbol set)
  (let ((ell (enum-set->list set)))
    (when (memq 'TRANSLIT ell)
      (set! ell (remq 'TRANSLIT ell)))
    (when (memq 'IGNORE ell)
      (set! ell (remq 'IGNORE ell)))
    (assert (= 1 (length ell)))
    (car ell)))

(define (iconv-encoding-aliases? set1 set2)
  (let ((sym1 (%set->encoding-symbol set1))
	(sym2 (%set->encoding-symbol set2)))
    (if (memq sym2 (case sym1
		     ((ANSI_X3.4-1968 ANSI_X3.4-1986 ASCII CP367 IBM367 ISO-IR-6 ISO646-US ISO_646.IRV:1991 US US-ASCII CSASCII)
		      '(ANSI_X3.4-1968 ANSI_X3.4-1986 ASCII CP367 IBM367 ISO-IR-6 ISO646-US ISO_646.IRV:1991 US US-ASCII CSASCII))
		     ((UTF-8)
		      '(UTF-8))
		     ((ISO-10646-UCS-2 UCS-2 CSUNICODE)
		      '(ISO-10646-UCS-2 UCS-2 CSUNICODE))
		     ((UCS-2BE UNICODE-1-1 UNICODEBIG CSUNICODE11)
		      '(UCS-2BE UNICODE-1-1 UNICODEBIG CSUNICODE11))
		     ((UCS-2LE UNICODELITTLE)
		      '(UCS-2LE UNICODELITTLE))
		     ((ISO-10646-UCS-4 UCS-4 CSUCS4)
		      '(ISO-10646-UCS-4 UCS-4 CSUCS4))
		     ((UCS-4BE)
		      '(UCS-4BE))
		     ((UCS-4LE)
		      '(UCS-4LE))
		     ((UTF-16)
		      '(UTF-16))
		     ((UTF-16BE)
		      '(UTF-16BE))
		     ((UTF-16LE)
		      '(UTF-16LE))
		     ((UTF-32)
		      '(UTF-32))
		     ((UTF-32BE)
		      '(UTF-32BE))
		     ((UTF-32LE)
		      '(UTF-32LE))
		     ((UNICODE-1-1-UTF-7 UTF-7 CSUNICODE11UTF7)
		      '(UNICODE-1-1-UTF-7 UTF-7 CSUNICODE11UTF7))
		     ((UCS-2-INTERNAL)
		      '(UCS-2-INTERNAL))
		     ((UCS-2-SWAPPED)
		      '(UCS-2-SWAPPED))
		     ((UCS-4-INTERNAL)
		      '(UCS-4-INTERNAL))
		     ((UCS-4-SWAPPED)
		      '(UCS-4-SWAPPED))
		     ((C99)
		      '(C99))
		     ((JAVA)
		      '(JAVA))
		     ((CP819 IBM819 ISO-8859-1 ISO-IR-100 ISO8859-1 ISO_8859-1 ISO_8859-1:1987 L1 LATIN1 CSISOLATIN1)
		      '(CP819 IBM819 ISO-8859-1 ISO-IR-100 ISO8859-1 ISO_8859-1 ISO_8859-1:1987 L1 LATIN1 CSISOLATIN1))
		     ((ISO-8859-2 ISO-IR-101 ISO8859-2 ISO_8859-2 ISO_8859-2:1987 L2 LATIN2 CSISOLATIN2)
		      '(ISO-8859-2 ISO-IR-101 ISO8859-2 ISO_8859-2 ISO_8859-2:1987 L2 LATIN2 CSISOLATIN2))
		     ((ISO-8859-3 ISO-IR-109 ISO8859-3 ISO_8859-3 ISO_8859-3:1988 L3 LATIN3 CSISOLATIN3)
		      '(ISO-8859-3 ISO-IR-109 ISO8859-3 ISO_8859-3 ISO_8859-3:1988 L3 LATIN3 CSISOLATIN3))
		     ((ISO-8859-4 ISO-IR-110 ISO8859-4 ISO_8859-4 ISO_8859-4:1988 L4 LATIN4 CSISOLATIN4)
		      '(ISO-8859-4 ISO-IR-110 ISO8859-4 ISO_8859-4 ISO_8859-4:1988 L4 LATIN4 CSISOLATIN4))
		     ((CYRILLIC ISO-8859-5 ISO-IR-144 ISO8859-5 ISO_8859-5 ISO_8859-5:1988 CSISOLATINCYRILLIC)
		      '(CYRILLIC ISO-8859-5 ISO-IR-144 ISO8859-5 ISO_8859-5 ISO_8859-5:1988 CSISOLATINCYRILLIC))
		     ((ARABIC ASMO-708 ECMA-114 ISO-8859-6 ISO-IR-127 ISO8859-6 ISO_8859-6 ISO_8859-6:1987 CSISOLATINARABIC)
		      '(ARABIC ASMO-708 ECMA-114 ISO-8859-6 ISO-IR-127 ISO8859-6 ISO_8859-6 ISO_8859-6:1987 CSISOLATINARABIC))
		     ((ECMA-118 ELOT_928 GREEK GREEK8 ISO-8859-7 ISO-IR-126 ISO8859-7 ISO_8859-7 ISO_8859-7:1987 ISO_8859-7:2003 CSISOLATINGREEK)
		      '(ECMA-118 ELOT_928 GREEK GREEK8 ISO-8859-7 ISO-IR-126 ISO8859-7 ISO_8859-7 ISO_8859-7:1987 ISO_8859-7:2003 CSISOLATINGREEK))
		     ((HEBREW ISO-8859-8 ISO-IR-138 ISO8859-8 ISO_8859-8 ISO_8859-8:1988 CSISOLATINHEBREW)
		      '(HEBREW ISO-8859-8 ISO-IR-138 ISO8859-8 ISO_8859-8 ISO_8859-8:1988 CSISOLATINHEBREW))
		     ((ISO-8859-9 ISO-IR-148 ISO8859-9 ISO_8859-9 ISO_8859-9:1989 L5 LATIN5 CSISOLATIN5)
		      '(ISO-8859-9 ISO-IR-148 ISO8859-9 ISO_8859-9 ISO_8859-9:1989 L5 LATIN5 CSISOLATIN5))
		     ((ISO-8859-10 ISO-IR-157 ISO8859-10 ISO_8859-10 ISO_8859-10:1992 L6 LATIN6 CSISOLATIN6)
		      '(ISO-8859-10 ISO-IR-157 ISO8859-10 ISO_8859-10 ISO_8859-10:1992 L6 LATIN6 CSISOLATIN6))
		     ((ISO-8859-11 ISO8859-11 ISO_8859-11)
		      '(ISO-8859-11 ISO8859-11 ISO_8859-11))
		     ((ISO-8859-13 ISO-IR-179 ISO8859-13 ISO_8859-13 L7 LATIN7)
		      '(ISO-8859-13 ISO-IR-179 ISO8859-13 ISO_8859-13 L7 LATIN7))
		     ((ISO-8859-14 ISO-CELTIC ISO-IR-199 ISO8859-14 ISO_8859-14 ISO_8859-14:1998 L8 LATIN8)
		      '(ISO-8859-14 ISO-CELTIC ISO-IR-199 ISO8859-14 ISO_8859-14 ISO_8859-14:1998 L8 LATIN8))
		     ((ISO-8859-15 ISO-IR-203 ISO8859-15 ISO_8859-15 ISO_8859-15:1998 LATIN-9)
		      '(ISO-8859-15 ISO-IR-203 ISO8859-15 ISO_8859-15 ISO_8859-15:1998 LATIN-9))
		     ((ISO-8859-16 ISO-IR-226 ISO8859-16 ISO_8859-16 ISO_8859-16:2001 L10 LATIN10)
		      '(ISO-8859-16 ISO-IR-226 ISO8859-16 ISO_8859-16 ISO_8859-16:2001 L10 LATIN10))
		     ((KOI8-R CSKOI8R)
		      '(KOI8-R CSKOI8R))
		     ((KOI8-U)
		      '(KOI8-U))
		     ((KOI8-RU)
		      '(KOI8-RU))
		     ((CP1250 MS-EE WINDOWS-1250)
		      '(CP1250 MS-EE WINDOWS-1250))
		     ((CP1251 MS-CYRL WINDOWS-1251)
		      '(CP1251 MS-CYRL WINDOWS-1251))
		     ((CP1252 MS-ANSI WINDOWS-1252)
		      '(CP1252 MS-ANSI WINDOWS-1252))
		     ((CP1253 MS-GREEK WINDOWS-1253)
		      '(CP1253 MS-GREEK WINDOWS-1253))
		     ((CP1254 MS-TURK WINDOWS-1254)
		      '(CP1254 MS-TURK WINDOWS-1254))
		     ((CP1255 MS-HEBR WINDOWS-1255)
		      '(CP1255 MS-HEBR WINDOWS-1255))
		     ((CP1256 MS-ARAB WINDOWS-1256)
		      '(CP1256 MS-ARAB WINDOWS-1256))
		     ((CP1257 WINBALTRIM WINDOWS-1257)
		      '(CP1257 WINBALTRIM WINDOWS-1257))
		     ((CP1258 WINDOWS-1258)
		      '(CP1258 WINDOWS-1258))
		     ((CP850 IBM850 CSPC850MULTILINGUAL)
		      '(CP850 IBM850 CSPC850MULTILINGUAL))
		     ((CP862 IBM862 CSPC862LATINHEBREW)
		      '(CP862 IBM862 CSPC862LATINHEBREW))
		     ((CP866 IBM866 CSIBM866)
		      '(CP866 IBM866 CSIBM866))
		     ((CP1131)
		      '(CP1131))
		     ((MAC MACINTOSH MACROMAN CSMACINTOSH)
		      '(MAC MACINTOSH MACROMAN CSMACINTOSH))
		     ((MACCENTRALEUROPE)
		      '(MACCENTRALEUROPE))
		     ((MACICELAND)
		      '(MACICELAND))
		     ((MACCROATIAN)
		      '(MACCROATIAN))
		     ((MACROMANIA)
		      '(MACROMANIA))
		     ((MACCYRILLIC)
		      '(MACCYRILLIC))
		     ((MACUKRAINE)
		      '(MACUKRAINE))
		     ((MACGREEK)
		      '(MACGREEK))
		     ((MACTURKISH)
		      '(MACTURKISH))
		     ((MACHEBREW)
		      '(MACHEBREW))
		     ((MACARABIC)
		      '(MACARABIC))
		     ((MACTHAI)
		      '(MACTHAI))
		     ((HP-ROMAN8 R8 ROMAN8 CSHPROMAN8)
		      '(HP-ROMAN8 R8 ROMAN8 CSHPROMAN8))
		     ((NEXTSTEP)
		      '(NEXTSTEP))
		     ((ARMSCII-8)
		      '(ARMSCII-8))
		     ((GEORGIAN-ACADEMY)
		      '(GEORGIAN-ACADEMY))
		     ((GEORGIAN-PS)
		      '(GEORGIAN-PS))
		     ((KOI8-T)
		      '(KOI8-T))
		     ((CP154 CYRILLIC-ASIAN PT154 PTCP154 CSPTCP154)
		      '(CP154 CYRILLIC-ASIAN PT154 PTCP154 CSPTCP154))
		     ((KZ-1048 RK1048 STRK1048-2002 CSKZ1048)
		      '(KZ-1048 RK1048 STRK1048-2002 CSKZ1048))
		     ((MULELAO-1)
		      '(MULELAO-1))
		     ((CP1133 IBM-CP1133)
		      '(CP1133 IBM-CP1133))
		     ((ISO-IR-166 TIS-620 TIS620 TIS620-0 TIS620.2529-1 TIS620.2533-0 TIS620.2533-1)
		      '(ISO-IR-166 TIS-620 TIS620 TIS620-0 TIS620.2529-1 TIS620.2533-0 TIS620.2533-1))
		     ((CP874 WINDOWS-874)
		      '(CP874 WINDOWS-874))
		     ((VISCII VISCII1.1-1 CSVISCII)
		      '(VISCII VISCII1.1-1 CSVISCII))
		     ((TCVN TCVN-5712 TCVN5712-1 TCVN5712-1:1993)
		      '(TCVN TCVN-5712 TCVN5712-1 TCVN5712-1:1993))
		     ((ISO-IR-14 ISO646-JP JIS_C6220-1969-RO JP CSISO14JISC6220RO)
		      '(ISO-IR-14 ISO646-JP JIS_C6220-1969-RO JP CSISO14JISC6220RO))
		     ((JISX0201-1976 JIS_X0201 X0201 CSHALFWIDTHKATAKANA)
		      '(JISX0201-1976 JIS_X0201 X0201 CSHALFWIDTHKATAKANA))
		     ((ISO-IR-87 JIS0208 JIS_C6226-1983 JIS_X0208 JIS_X0208-1983 JIS_X0208-1990 X0208 CSISO87JISX0208)
		      '(ISO-IR-87 JIS0208 JIS_C6226-1983 JIS_X0208 JIS_X0208-1983 JIS_X0208-1990 X0208 CSISO87JISX0208))
		     ((ISO-IR-159 JIS_X0212 JIS_X0212-1990 JIS_X0212.1990-0 X0212 CSISO159JISX02121990)
		      '(ISO-IR-159 JIS_X0212 JIS_X0212-1990 JIS_X0212.1990-0 X0212 CSISO159JISX02121990))
		     ((CN GB_1988-80 ISO-IR-57 ISO646-CN CSISO57GB1988)
		      '(CN GB_1988-80 ISO-IR-57 ISO646-CN CSISO57GB1988))
		     ((CHINESE GB_2312-80 ISO-IR-58 CSISO58GB231280)
		      '(CHINESE GB_2312-80 ISO-IR-58 CSISO58GB231280))
		     ((CN-GB-ISOIR165 ISO-IR-165)
		      '(CN-GB-ISOIR165 ISO-IR-165))
		     ((ISO-IR-149 KOREAN KSC_5601 KS_C_5601-1987 KS_C_5601-1989 CSKSC56011987)
		      '(ISO-IR-149 KOREAN KSC_5601 KS_C_5601-1987 KS_C_5601-1989 CSKSC56011987))
		     ((EUC-JP EUCJP EXTENDED_UNIX_CODE_PACKED_FORMAT_FOR_JAPANESE CSEUCPKDFMTJAPANESE)
		      '(EUC-JP EUCJP EXTENDED_UNIX_CODE_PACKED_FORMAT_FOR_JAPANESE CSEUCPKDFMTJAPANESE))
		     ((MS_KANJI SHIFT-JIS SHIFT_JIS SJIS CSSHIFTJIS)
		      '(MS_KANJI SHIFT-JIS SHIFT_JIS SJIS CSSHIFTJIS))
		     ((CP932)
		      '(CP932))
		     ((ISO-2022-JP CSISO2022JP)
		      '(ISO-2022-JP CSISO2022JP))
		     ((ISO-2022-JP-1)
		      '(ISO-2022-JP-1))
		     ((ISO-2022-JP-2 CSISO2022JP2)
		      '(ISO-2022-JP-2 CSISO2022JP2))
		     ((CN-GB EUC-CN EUCCN GB2312 CSGB2312)
		      '(CN-GB EUC-CN EUCCN GB2312 CSGB2312))
		     ((GBK)
		      '(GBK))
		     ((CP936 MS936 WINDOWS-936)
		      '(CP936 MS936 WINDOWS-936))
		     ((GB18030)
		      '(GB18030))
		     ((ISO-2022-CN CSISO2022CN)
		      '(ISO-2022-CN CSISO2022CN))
		     ((ISO-2022-CN-EXT)
		      '(ISO-2022-CN-EXT))
		     ((HZ HZ-GB-2312)
		      '(HZ HZ-GB-2312))
		     ((EUC-TW EUCTW CSEUCTW)
		      '(EUC-TW EUCTW CSEUCTW))
		     ((BIG-5 BIG-FIVE BIG5 BIGFIVE CN-BIG5 CSBIG5)
		      '(BIG-5 BIG-FIVE BIG5 BIGFIVE CN-BIG5 CSBIG5))
		     ((CP950)
		      '(CP950))
		     ((BIG5-HKSCS:1999)
		      '(BIG5-HKSCS:1999))
		     ((BIG5-HKSCS:2001)
		      '(BIG5-HKSCS:2001))
		     ((BIG5-HKSCS BIG5-HKSCS:2004 BIG5HKSCS)
		      '(BIG5-HKSCS BIG5-HKSCS:2004 BIG5HKSCS))
		     ((EUC-KR EUCKR CSEUCKR)
		      '(EUC-KR EUCKR CSEUCKR))
		     ((CP949 UHC)
		      '(CP949 UHC))
		     ((CP1361 JOHAB)
		      '(CP1361 JOHAB))
		     ((ISO-2022-KR CSISO2022KR)
		      '(ISO-2022-KR CSISO2022KR))))
		     #t #f)))

(define (iconv-encoding=? set1 set2)
  (define-syntax both
    (syntax-rules ()
      ((_ ?expr1 ?expr2)
       (let ((a ?expr1)
	     (b ?expr2))
	 (or (and a b) (and (not a) (not b)))))))
  (or (enum-set=? set1 set2)
      (and (both (enum-set-member? 'TRANSLIT set1)
		 (enum-set-member? 'TRANSLIT set2))
	   (both (enum-set-member? 'IGNORE set1)
		 (enum-set-member? 'IGNORE set2))
	   (iconv-encoding-aliases? set1 set2))))

;;; --------------------------------------------------------------------

(define-struct iconv
  (pointer from to))

(define (%struct-iconv-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[iconv")
  (%display " pointer=")	(%display (iconv-pointer S))
  (%display " from-encoding=")	(%display (enum-set->list (iconv-from S)))
  (%display " to-encoding=")	(%display (enum-set->list (iconv-to   S)))
  (%display "]"))

(define %iconv-guardian
  (make-guardian))

(define (%free-allocated-iconv)
  (do ((handle (%iconv-guardian) (%iconv-guardian)))
      ((not handle))
    (let ((P (iconv-pointer handle)))
      (unless (ffi.pointer-null? P)
	(capi.glibc-iconv-close P)))))

(define (iconv-open from to)
  (define who 'iconv-open)
  (define (main to from)
    (with-arguments-validation (who)
	((iconv-set	from)
	 (iconv-set	to))
      (let ((rv (capi.glibc-iconv-open (%enum-set->string from who)
				       (%enum-set->string to   who))))
	(if (ffi.pointer? rv)
	    (%iconv-guardian (make-iconv rv from to))
	  (raise-errno-error who rv to from)))))
  (define (%enum-set->string set who)
    (let ((ell	(enum-set->list set))
	  (spec	""))
      (when (memq 'TRANSLIT ell)
	(set! spec "//TRANSLIT")
	(set! ell (remq 'TRANSLIT ell)))
      (when (memq 'IGNORE ell)
	(set! spec (string-append spec "//IGNORE"))
	(set! ell (remq 'IGNORE ell)))
      (if (= 1 (length ell))
	  (string->latin1 (string-append (symbol->string (car ell)) spec))
	(assertion-violation who "invalid Iconv enumeration set" set))))
  (main to from))

(define (iconv-close handle)
  (define who 'iconv-close)
  (with-arguments-validation (who)
      ((iconv	handle))
    ;;Close the handle and mutate the pointer object to NULL.
    (let ((rv (capi.glibc-iconv-close (iconv-pointer handle))))
      (unless (unsafe.fxzero? rv)
	(raise-errno-error who rv handle)))))

(define (iconv-closed? handle)
  (define who 'iconv-closed?)
  (with-arguments-validation (who)
      ((iconv	handle))
    (ffi.pointer-null? (iconv-pointer handle))))

(define (iconv! handle
		input  input.start  input.past
		output output.start output.past)
  (define who 'iconv!)
  (with-arguments-validation (who)
      ((iconv		handle)
       (bytevector	input)
       (bytevector	output)
       (index		input.start)
       (false/index	input.past)
       (index		output.start)
       (false/index	output.past))
    (let ((rv (capi.glibc-iconv (iconv-pointer handle)
				input  input.start  input.past
				output output.start output.past)))
      (if (pair? rv)
	  (values (car rv) (cdr rv))
	(raise-errno-error who rv handle
			   input  input.start  input.past
			   output output.start output.past)))))


;;;; done

(post-gc-hooks (cons* %free-allocated-regex
		      %free-allocated-iconv
		      (post-gc-hooks)))

(set-rtd-printer! (type-descriptor iconv) %struct-iconv-printer)

)

;;; end of file
;; Local Variables:
;; eval: (put 'with-bytevectors 'scheme-indent-function 1)
;; End:
