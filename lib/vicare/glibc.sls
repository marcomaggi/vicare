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
;;;Copyright (C) 2011, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
    cond-expand

    ;; operating system environment variables
    clearenv

    ;; file system directories
    dirfd

    ;; temporary files and directories
    mkstemp			mkdtemp

    ;; file system synchronisation
    sync			fsync
    fdatasync

    ;; sockets and networking
    if-nametoindex		if-indextoname
    if-nameindex
    IN_CLASSA			IN_CLASSB
    IN_CLASSC			IN_CLASSD
    IN_MULTICAST		IN_EXPERIMENTAL
    IN_BADCLASS

    IN6_IS_ADDR_UNSPECIFIED	IN6_IS_ADDR_LOOPBACK
    IN6_IS_ADDR_LINKLOCAL	IN6_IS_ADDR_SITELOCAL
    IN6_IS_ADDR_V4MAPPED	IN6_IS_ADDR_V4COMPAT
    IN6_IS_ADDR_MULTICAST	IN6_IS_ADDR_MC_NODELOCAL
    IN6_IS_ADDR_MC_LINKLOCAL	IN6_IS_ADDR_MC_SITELOCAL
    IN6_IS_ADDR_MC_ORGLOCAL	IN6_IS_ADDR_MC_GLOBAL
    IN6_ARE_ADDR_EQUAL

    bindresvport		bindresvport6

    ;; mathematics
    csin		ccos		ctan
    casin		cacos		catan
    cexp		clog		clog10
    csqrt		cpow
    sinh		cosh		tanh
    asinh		acosh		atanh
    csinh		ccosh		ctanh
    casinh		cacosh		catanh
    erf			erfc		tgamma		lgamma
    j0			j1		y0
    y1			jn		yn

    ;; random numbers
    rand		srand

    ;; pattern matching, globbing, regular expressions
    fnmatch		glob		glob/string
    regcomp		regcomp/disown
    regexec		regfree

    ;; word expansion
    wordexp		wordexp/string
    )
  (import (except (vicare)
		  sinh		cosh		tanh
		  asinh		acosh		atanh)
    (prefix (only (vicare posix)
		  directory-stream?
		  directory-stream-closed?
		  directory-stream-pointer)
	    posix.)
    (vicare language-extensions syntaxes)
    (vicare platform constants)
    (prefix (vicare unsafe capi)
	    capi.)
    (prefix (vicare platform words)
	    words.)
    (vicare arguments validation)
    (vicare unsafe operations)
    (vicare language-extensions cond-expand))


;;;; helpers

(define (raise-errno-error who errno . irritants)
  (raise (condition
	  (make-error)
	  (make-errno-condition errno)
	  (make-who-condition who)
	  (make-message-condition (strerror errno))
	  (make-irritants-condition irritants))))


;;;; arguments validation

(define-argument-validation (string/bytevector who obj)
  (or (string? obj) (bytevector? obj))
  (assertion-violation who "expected string or bytevector as argument" obj))

(define-argument-validation (false/procedure who obj)
  (or (not obj) (procedure? obj))
  (assertion-violation who "expected false or procedure as argument" obj))

(define-argument-validation (index who obj)
  (and (fixnum? obj) ($fx<= 0 obj))
  (assertion-violation who "expected non-negative fixnum as argument" obj))

(define-argument-validation (false/index who obj)
  (or (not obj) (and (fixnum? obj) ($fx<= 0 obj)))
  (assertion-violation who "expected false or non-negative fixnum as argument" obj))

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
  (and (fixnum? obj) ($fx<= 0 obj))
  (assertion-violation who "expected fixnum file descriptor as argument" obj))


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
      (if ($fx<= 0 rv)
	  rv
	(raise-errno-error who rv stream)))))


;;;; temporary files and directories

(define (mkstemp template)
  (define who 'mkstemp)
  (with-arguments-validation (who)
      ((bytevector  template))
    (let ((rv (capi.glibc-mkstemp template)))
      (if ($fx<= 0 rv)
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
    (unless ($fxzero? rv)
      (raise-errno-error who rv))))

(define (fsync fd)
  (define who 'fsync)
  (with-arguments-validation (who)
      ((file-descriptor  fd))
    (let ((rv (capi.glibc-fsync fd)))
      (unless ($fxzero? rv)
	(raise-errno-error who rv fd)))))

(define (fdatasync fd)
  (define who 'fdatasync)
  (with-arguments-validation (who)
      ((file-descriptor  fd))
    (let ((rv (capi.glibc-fdatasync fd)))
      (unless ($fxzero? rv)
	(raise-errno-error who rv fd)))))


;;;; sockets and networking

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

;;; --------------------------------------------------------------------

(define (IN_CLASSA addr)
  (define who 'IN_CLASSA)
  (with-arguments-validation (who)
      ((word-u32	addr))
    (capi.glibc-IN_CLASSA addr)))

(define (IN_CLASSB addr)
  (define who 'IN_CLASSB)
  (with-arguments-validation (who)
      ((word-u32	addr))
    (capi.glibc-IN_CLASSB addr)))

(define (IN_CLASSC addr)
  (define who 'IN_CLASSC)
  (with-arguments-validation (who)
      ((word-u32	addr))
    (capi.glibc-IN_CLASSC addr)))

(define (IN_CLASSD addr)
  (define who 'IN_CLASSD)
  (with-arguments-validation (who)
      ((word-u32	addr))
    (capi.glibc-IN_CLASSD addr)))

(define (IN_MULTICAST addr)
  (define who 'IN_MULTICAST)
  (with-arguments-validation (who)
      ((word-u32	addr))
    (capi.glibc-IN_MULTICAST addr)))

(define (IN_EXPERIMENTAL addr)
  (define who 'IN_EXPERIMENTAL)
  (with-arguments-validation (who)
      ((word-u32	addr))
    (capi.glibc-IN_EXPERIMENTAL addr)))

(define (IN_BADCLASS addr)
  (define who 'IN_BADCLASS)
  (with-arguments-validation (who)
      ((word-u32	addr))
    (capi.glibc-IN_BADCLASS addr)))

;;; --------------------------------------------------------------------

(define (IN6_IS_ADDR_UNSPECIFIED addr)
  (define who 'IN6_IS_ADDR_UNSPECIFIED)
  (with-arguments-validation (who)
      ((bytevector	addr))
    (capi.glibc-IN6_IS_ADDR_UNSPECIFIED addr)))

(define (IN6_IS_ADDR_LOOPBACK addr)
  (define who 'IN6_IS_ADDR_LOOPBACK)
  (with-arguments-validation (who)
      ((bytevector	addr))
    (capi.glibc-IN6_IS_ADDR_LOOPBACK addr)))

(define (IN6_IS_ADDR_LINKLOCAL addr)
  (define who 'IN6_IS_ADDR_LINKLOCAL)
  (with-arguments-validation (who)
      ((bytevector	addr))
    (capi.glibc-IN6_IS_ADDR_LINKLOCAL addr)))

(define (IN6_IS_ADDR_SITELOCAL addr)
  (define who 'IN6_IS_ADDR_SITELOCAL)
  (with-arguments-validation (who)
      ((bytevector	addr))
    (capi.glibc-IN6_IS_ADDR_SITELOCAL addr)))

(define (IN6_IS_ADDR_V4MAPPED addr)
  (define who 'IN6_IS_ADDR_V4MAPPED)
  (with-arguments-validation (who)
      ((bytevector	addr))
    (capi.glibc-IN6_IS_ADDR_V4MAPPED addr)))

(define (IN6_IS_ADDR_V4COMPAT addr)
  (define who 'IN6_IS_ADDR_V4COMPAT)
  (with-arguments-validation (who)
      ((bytevector	addr))
    (capi.glibc-IN6_IS_ADDR_V4COMPAT addr)))

(define (IN6_IS_ADDR_MULTICAST addr)
  (define who 'IN6_IS_ADDR_MULTICAST)
  (with-arguments-validation (who)
      ((bytevector	addr))
    (capi.glibc-IN6_IS_ADDR_MULTICAST addr)))

(define (IN6_IS_ADDR_MC_NODELOCAL addr)
  (define who 'IN6_IS_ADDR_MC_NODELOCAL)
  (with-arguments-validation (who)
      ((bytevector	addr))
    (capi.glibc-IN6_IS_ADDR_MC_NODELOCAL addr)))

(define (IN6_IS_ADDR_MC_LINKLOCAL addr)
  (define who 'IN6_IS_ADDR_MC_LINKLOCAL)
  (with-arguments-validation (who)
      ((bytevector	addr))
    (capi.glibc-IN6_IS_ADDR_MC_LINKLOCAL addr)))

(define (IN6_IS_ADDR_MC_SITELOCAL addr)
  (define who 'IN6_IS_ADDR_MC_SITELOCAL)
  (with-arguments-validation (who)
      ((bytevector	addr))
    (capi.glibc-IN6_IS_ADDR_MC_SITELOCAL addr)))

(define (IN6_IS_ADDR_MC_ORGLOCAL addr)
  (define who 'IN6_IS_ADDR_MC_ORGLOCAL)
  (with-arguments-validation (who)
      ((bytevector	addr))
    (capi.glibc-IN6_IS_ADDR_MC_ORGLOCAL addr)))

(define (IN6_IS_ADDR_MC_GLOBAL addr)
  (define who 'IN6_IS_ADDR_MC_GLOBAL)
  (with-arguments-validation (who)
      ((bytevector	addr))
    (capi.glibc-IN6_IS_ADDR_MC_GLOBAL addr)))

(define (IN6_ARE_ADDR_EQUAL addr1 addr2)
  (define who 'IN6_ARE_ADDR_EQUAL)
  (with-arguments-validation (who)
      ((bytevector	addr1)
       (bytevector	addr2))
    (capi.glibc-IN6_ARE_ADDR_EQUAL addr1 addr2)))

;;; --------------------------------------------------------------------

(define (bindresvport fd addr)
  (define who 'bindresvport)
  (with-arguments-validation (who)
      ((file-descriptor	fd)
       (bytevector	addr))
    (let ((rv (capi.glibc-bindresvport fd addr)))
      (when rv
	(raise-errno-error who rv fd addr)))))

(define (bindresvport6 fd addr)
  (define who 'bindresvport6)
  (with-arguments-validation (who)
      ((file-descriptor	fd)
       (bytevector	addr))
    (let ((rv (capi.glibc-bindresvport6 fd addr)))
      (when rv
	(raise-errno-error who rv fd addr)))))


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

(define-one-operand/flonum sinh		capi.glibc-sinh)
(define-one-operand/flonum cosh		capi.glibc-cosh)
(define-one-operand/flonum tanh		capi.glibc-tanh)
(define-one-operand/flonum asinh	capi.glibc-asinh)
(define-one-operand/flonum acosh	capi.glibc-acosh)
(define-one-operand/flonum atanh	capi.glibc-atanh)
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
  (do ((pointer (%regex-guardian) (%regex-guardian)))
      ((not pointer))
    (capi.glibc-regfree pointer)))

(define (regcomp pattern flags)
  (%regex-guardian (%regcomp 'regcomp pattern flags)))

(define (regcomp/disown pattern flags)
  (%regcomp 'regcomp/disown pattern flags))

(define (%regcomp who pattern flags)
  (with-arguments-validation (who)
      ((string/bytevector	pattern)
       (fixnum			flags))
    (with-bytevectors ((pattern.bv pattern))
      (let ((rv (capi.glibc-regcomp pattern.bv flags)))
	(cond ((pointer? rv)
	       rv)
	      ((not rv)
	       (error who
		 "error allocating memory for precompiled regular expression"
		 pattern flags))
	      (else
	       (error who (latin1->string (cdr rv)) (car rv) pattern flags)))))))

(define (regexec regex string flags)
  (define who 'regexec)
  (with-arguments-validation (who)
      ((pointer			regex)
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
      ((pointer  regex))
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


;;;; features cond-expand

(define-cond-expand cond-expand
  (let ()
    (import (vicare platform features)
      (only (vicare language-extensions cond-expand helpers)
	    define-cond-expand-identifiers-helper))
    (define-cond-expand-identifiers-helper vicare-glibc-features
      (iconv				HAVE_ICONV)
      (clearenv				HAVE_CLEARENV)
      (dirfd				HAVE_DIRFD)
      (mkstemp				HAVE_MKSTEMP)
      (mkdtemp				HAVE_MKDTEMP)
      (sync				HAVE_SYNC)
      (fsync				HAVE_FSYNC)
      (fdatasync			HAVE_FDATASYNC)
      (if-nametoindex			HAVE_IF_NAMETOINDEX)
      (if-indextoname			HAVE_IF_INDEXTONAME)
      (if-nameindex			HAVE_IF_NAMEINDEX)
      (IN_CLASSA			HAVE_IN_CLASSA)
      (IN_CLASSB			HAVE_IN_CLASSB)
      (IN_CLASSC			HAVE_IN_CLASSC)
      (IN_CLASSD			HAVE_IN_CLASSD)
      (IN_MULTICAST			HAVE_IN_MULTICAST)
      (IN_EXPERIMENTAL			HAVE_IN_EXPERIMENTAL)
      (IN_BADCLASS			HAVE_IN_BADCLASS)
      (IN6_IS_ADDR_UNSPECIFIED		HAVE_IN6_IS_ADDR_UNSPECIFIED)
      (IN6_IS_ADDR_LOOPBACK		HAVE_IN6_IS_ADDR_LOOPBACK)
      (IN6_IS_ADDR_LINKLOCAL		HAVE_IN6_IS_ADDR_LINKLOCAL)
      (IN6_IS_ADDR_SITELOCAL		HAVE_IN6_IS_ADDR_SITELOCAL)
      (IN6_IS_ADDR_V4MAPPED		HAVE_IN6_IS_ADDR_V4MAPPED)
      (IN6_IS_ADDR_V4COMPAT		HAVE_IN6_IS_ADDR_V4COMPAT)
      (IN6_IS_ADDR_MULTICAST		HAVE_IN6_IS_ADDR_MULTICAST)
      (IN6_IS_ADDR_MC_NODELOCAL		HAVE_IN6_IS_ADDR_MC_NODELOCAL)
      (IN6_IS_ADDR_MC_LINKLOCAL		HAVE_IN6_IS_ADDR_MC_LINKLOCAL)
      (IN6_IS_ADDR_MC_SITELOCAL		HAVE_IN6_IS_ADDR_MC_SITELOCAL)
      (IN6_IS_ADDR_MC_ORGLOCAL		HAVE_IN6_IS_ADDR_MC_ORGLOCAL)
      (IN6_IS_ADDR_MC_GLOBAL		HAVE_IN6_IS_ADDR_MC_GLOBAL)
      (IN6_ARE_ADDR_EQUAL		HAVE_IN6_ARE_ADDR_EQUAL)
      (bindresvport			HAVE_BINDRESVPORT)
      (bindresvport6			HAVE_BINDRESVPORT6)
      (csin				HAVE_CSIN)
      (ccos				HAVE_CCOS)
      (ctan				HAVE_CTAN)
      (casin				HAVE_CASIN)
      (cacos				HAVE_CACOS)
      (catan				HAVE_CATAN)
      (cexp				HAVE_CEXP)
      (clog				HAVE_CLOG)
      (clog10				HAVE_CLOG10)
      (csqrt				HAVE_CSQRT)
      (cpow				HAVE_CPOW)
      (sinh				HAVE_SINH)
      (cosh				HAVE_COSH)
      (tanh				HAVE_TANH)
      (csinh				HAVE_CSINH)
      (ccosh				HAVE_CCOSH)
      (ctanh				HAVE_CTANH)
      (asinh				HAVE_ASINH)
      (acosh				HAVE_ACOSH)
      (atanh				HAVE_ATANH)
      (casinh				HAVE_CASINH)
      (cacosh				HAVE_CACOSH)
      (catanh				HAVE_CATANH)
      (erf				HAVE_ERF)
      (erfc				HAVE_ERFC)
      (lgamma				HAVE_LGAMMA_R)
      (tgamma				HAVE_TGAMMA)
      (y0				HAVE_Y0)
      (y1				HAVE_Y1)
      (j0				HAVE_J0)
      (j1				HAVE_J1)
      (yn				HAVE_YN)
      (jn				HAVE_JN)
      (rand				HAVE_RAND)
      (rand				HAVE_RAND)
      (fnmatch				HAVE_FNMATCH)
      (glob				HAVE_GLOB)
      (regcomp				HAVE_REGCOMP)
      (regcomp				HAVE_REGCOMP)
      (regcomp				HAVE_REGCOMP)
      (wordexp				HAVE_WORDEXP)
      (iconv				HAVE_ICONV)
      #| define-cond-expand-identifiers-helper |# )

    vicare-glibc-features))


;;;; done

(post-gc-hooks (cons* %free-allocated-regex
		      (post-gc-hooks)))

)

;;; end of file
;; Local Variables:
;; eval: (put 'with-bytevectors 'scheme-indent-function 1)
;; End:
