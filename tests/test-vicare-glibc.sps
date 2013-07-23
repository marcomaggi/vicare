;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: test implementation of GNU C Library functions
;;;Date: Wed Nov  9, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2011, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (vicare)
  (prefix (vicare posix)
	  px.)
  (prefix (vicare glibc)
	  glibc.)
  (prefix (vicare ffi)
	  ffi.)
  (vicare platform constants)
  (vicare language-extensions syntaxes)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare GNU C Library functions\n")


(parametrise ((check-test-name	'directories))

  (check
      (let* ((stream (px.opendir ".."))
	     (fd     (glibc.dirfd stream)))
	(px.closedir stream)
	(fixnum? fd))
    => #t)

  (let ((pwd (px.getcwd/string)))
    (check
	(begin
	  (px.mkdir "one" S_IRWXU)
	  (unwind-protect
	      (let ((stream (px.opendir "one")))
		(unwind-protect
		    (let ((fd (glibc.dirfd stream)))
		      (px.fchdir fd)
		      (px.getcwd/string))
		  (px.closedir stream)))
	    (px.chdir pwd)
	    (px.rmdir "one")))
      => (string-append pwd "/one")))

  #t)


(parametrise ((check-test-name	'temporary))

  (check
      (let* ((template	((string->filename-func) "/tmp/tmp.XXXXXX"))
	     (fd	(glibc.mkstemp template)))
;;;	(check-pretty-print ((filename->string-func) template))
	(unwind-protect
	    (fixnum? fd)
	  (px.unlink template)))
    => #t)

  (check
      (let* ((template	((string->filename-func) "/tmp/tmp.XXXXXX"))
	     (name	(glibc.mkdtemp template)))
;;;	(check-pretty-print ((filename->string-func) template))
;;;	(check-pretty-print ((filename->string-func) name))
	(unwind-protect
	    (bytevector? name)
	  (px.rmdir template)))
    => #t)

  #t)


(parametrise ((check-test-name	'sync))

  (check
      (begin
	(glibc.sync)
	#t)
    => #t)

  #t)


(parametrise ((check-test-name	'sockets))

  (check (glibc.if-indextoname 0)	=> #f)
  (check (glibc.if-indextoname 1)	=> "lo")

  (check (glibc.if-nametoindex "lo")	=> 1)

  ;;These are specific to the maintainer's system.
  (when #f
    (check (glibc.if-indextoname 2)		=> "eth0")
    (check (glibc.if-nametoindex "eth0")	=> 2))

  (when #t
    (check-pretty-print (list 'alist-of-ifaces (glibc.if-nameindex))))

;;; --------------------------------------------------------------------

  #t)


(parametrise ((check-test-name	'networking))

  (let ((host-address.class-A '#vu8(0 0 0 1))
	(host-address.class-B '#vu8(128 0 0 1))
	(host-address.class-C '#vu8(192 0 0 1)))

    (check
	(let ((addr (px.sockaddr_in.in_addr.number (px.make-sockaddr_in host-address.class-A 88))))
	  (glibc.IN_CLASSA addr))
      => #t)

    (check
    	(let ((addr (px.sockaddr_in.in_addr.number (px.make-sockaddr_in host-address.class-B 88))))
    	  (glibc.IN_CLASSA addr))
      => #f)

    (check
    	(let ((addr (px.sockaddr_in.in_addr.number (px.make-sockaddr_in host-address.class-C 88))))
    	  (glibc.IN_CLASSA addr))
      => #f)

    ;;;

    (check
    	(let ((addr (px.sockaddr_in.in_addr.number (px.make-sockaddr_in host-address.class-A 88))))
    	  (glibc.IN_CLASSB addr))
      => #f)

    (check
    	(let ((addr (px.sockaddr_in.in_addr.number (px.make-sockaddr_in host-address.class-B 88))))
    	  (glibc.IN_CLASSB addr))
      => #t)

    (check
    	(let ((addr (px.sockaddr_in.in_addr.number (px.make-sockaddr_in host-address.class-C 88))))
    	  (glibc.IN_CLASSB addr))
      => #f)

    ;;;

    (check
    	(let ((addr (px.sockaddr_in.in_addr.number (px.make-sockaddr_in host-address.class-A 88))))
    	  (glibc.IN_CLASSC addr))
      => #f)

    (check
    	(let ((addr (px.sockaddr_in.in_addr.number (px.make-sockaddr_in host-address.class-B 88))))
    	  (glibc.IN_CLASSC addr))
      => #f)

    (check
    	(let ((addr (px.sockaddr_in.in_addr.number (px.make-sockaddr_in host-address.class-C 88))))
    	  (glibc.IN_CLASSC addr))
      => #t)

    ;;;

    (check
    	(let ((addr (px.sockaddr_in.in_addr.number (px.make-sockaddr_in host-address.class-A 88))))
    	  (glibc.IN_CLASSD addr))
      => #f)

    (check
    	(let ((addr (px.sockaddr_in.in_addr.number (px.make-sockaddr_in host-address.class-B 88))))
    	  (glibc.IN_CLASSD addr))
      => #f)

    (check
    	(let ((addr (px.sockaddr_in.in_addr.number (px.make-sockaddr_in host-address.class-C 88))))
    	  (glibc.IN_CLASSD addr))
      => #f)

    ;;;

    (check
    	(let ((addr (px.sockaddr_in.in_addr.number (px.make-sockaddr_in host-address.class-A 88))))
    	  (glibc.IN_MULTICAST addr))
      => #f)

    ;;;

    (check
    	(let ((addr (px.sockaddr_in.in_addr.number (px.make-sockaddr_in host-address.class-A 88))))
    	  (glibc.IN_EXPERIMENTAL addr))
      => #f)

    ;;;

    (check
    	(let ((addr (px.sockaddr_in.in_addr.number (px.make-sockaddr_in host-address.class-A 88))))
    	  (glibc.IN_BADCLASS addr))
      => #f)

    #f)


  #t)


(parametrise ((check-test-name	'math))

  (define (almost= X Y)
    (define EPSILON 1e-6)
    (define (almost-re= x y)
      (or (= x y)
	  (fl<? (flabs (fl- x y)) EPSILON)))
    (and (almost-re= (real-part X)
		     (real-part Y))
	 (almost-re= (imag-part X)
		     (imag-part Y))))

;;; trigonometric functions

  (check (cflonum? (glibc.csin 1.2+3.4i))	=> #t)
  (check (glibc.csin 1.2+3.4i)	(=> almost=) (sin 1.2+3.4i))

  (check (cflonum? (glibc.ccos 1.2+3.4i))	=> #t)
  (check (glibc.ccos 1.2+3.4i)	(=> almost=) 5.434909-13.948304i)

  (check (cflonum? (glibc.ctan 1.2+3.4i))	=> #t)
  (check (glibc.ctan 1.2+3.4i)	(=> almost=) (tan 1.2+3.4i))

  (check (cflonum? (glibc.casin 1.2+3.4i))	=> #t)
  (check (glibc.casin 1.2+3.4i)	(=> almost=) (asin 1.2+3.4i))

  (check (cflonum? (glibc.cacos 1.2+3.4i))	=> #t)
  (check (glibc.cacos 1.2+3.4i)	(=> almost=) (acos 1.2+3.4i))

  (check (cflonum? (glibc.catan 1.2+3.4i))	=> #t)
  (check (glibc.catan 1.2+3.4i)	(=> almost=) (atan 1.2+3.4i))

  (check (cflonum? (glibc.cexp 1.2+3.4i))	=> #t)
  (check (glibc.cexp 1.2+3.4i)	(=> almost=) (exp 1.2+3.4i))

  (check (cflonum? (glibc.clog 1.2+3.4i))	=> #t)
  (check (glibc.clog 1.2+3.4i)	(=> almost=) (log 1.2+3.4i))

  (check (cflonum? (glibc.clog10 1.2+3.4i))	=> #t)
  (check (glibc.clog10 1.2+3.4i) (=> almost=) (log 1.2+3.4i 10.))

  (check (cflonum? (glibc.csqrt 1.2+3.4i))	=> #t)
  (check (glibc.csqrt 1.2+3.4i)	(=> almost=) (sqrt 1.2+3.4i))

  (check (cflonum? (glibc.cpow 1.2+3.4i 5.6+7.8i))	=> #t)
  (check (glibc.cpow 1.2+3.4i 5.6+7.8i)	(=> almost=) (expt 1.2+3.4i 5.6+7.8i))

;;; --------------------------------------------------------------------
;;; hyperbolic functions

  (check (flonum? (glibc.sinh 1.2))	=> #t)
  (check (glibc.sinh 1.2)	(=> almost=) 1.509461)

  (check (flonum? (glibc.cosh 1.2))	=> #t)
  (check (glibc.cosh 1.2)	(=> almost=) 1.810656)

  (check (flonum? (glibc.tanh 1.2))	=> #t)
  (check (glibc.tanh 1.2)	(=> almost=) 0.833655)

  (check (flonum? (glibc.asinh 1.2))	=> #t)
  (check (glibc.asinh 1.2)	(=> almost=) 1.015973)

  (check (flonum? (glibc.acosh 1.2))	=> #t)
  (check (glibc.acosh 1.2)	(=> almost=) 0.622363)

  (check (flonum? (glibc.atanh .2))	=> #t)
  (check (glibc.atanh 0.2)	(=> almost=) 0.202733)

  (check (cflonum? (glibc.csinh 1.2+3.4i))	=> #t)
  (check (glibc.csinh 1.2+3.4i)	(=> almost=) -1.459345-0.462697i)

  (check (cflonum? (glibc.ccosh 1.2+3.4i))	=> #t)
  (check (glibc.ccosh 1.2+3.4i)	(=> almost=) -1.750539-0.385729i)

  (check (cflonum? (glibc.ctanh 1.2+3.4i))	=> #t)
  (check (glibc.ctanh 1.2+3.4i) (=> almost=) 0.850597+0.076889i)

  (check (cflonum? (glibc.casinh 1.2+3.4i))	=> #t)
  (check (glibc.casinh 1.2+3.4i) (=> almost=) 1.960546+1.218869i)

  (check (cflonum? (glibc.cacosh 1.2+3.4i))	=> #t)
  (check (glibc.cacosh 1.2+3.4i) (=> almost=) 1.990465+1.243053i)

  (check (cflonum? (glibc.catanh 1.2+3.4i))	=> #t)
  (check (glibc.catanh 1.2+3.4i) (=> almost=) 0.086569+1.313022i)

;;; --------------------------------------------------------------------
;;; special functions

  (check (flonum? (glibc.erf 1.2))		=> #t)
  (check (flonum? (glibc.erfc 1.2))		=> #t)
  (check (flonum? (glibc.tgamma 1.2))		=> #t)
  (check (flonum? (glibc.j0 1.2))		=> #t)
  (check (flonum? (glibc.j1 1.2))		=> #t)
  (check (flonum? (glibc.y0 1.2))		=> #t)
  (check (flonum? (glibc.y1 1.2))		=> #t)
  (check (flonum? (glibc.jn 1 1.2))		=> #t)
  (check (flonum? (glibc.yn 1 1.2))		=> #t)

  (check
      (let-values (((y sgn) (glibc.lgamma 1.2)))
	(cons (flonum? y) (fixnum? sgn)))
    => '(#t . #t))

  #t)


(parametrise ((check-test-name	'random))

  (check
      (begin
	(glibc.srand 10)
	(integer? (glibc.rand)))
    => #t)

  #t)


(parametrise ((check-test-name	'match))

  (check
      (glibc.fnmatch "ciao" "ciao" 0)
    => #t)

  (check
      (glibc.fnmatch "ciao" "salut" 0)
    => #f)

  (check
      (glibc.fnmatch "ciao*" "ciao a tutti" 0)
    => #t)

  (check
      (glibc.fnmatch "*(Ciao)" "CiaoCiao" FNM_EXTMATCH)
    => #t)

  (check
      (glibc.fnmatch "?(Ciao|Hello)" "Hello" FNM_EXTMATCH)
    => #t)

;;; --------------------------------------------------------------------

  (check
      (glibc.glob/string "Makefile" 0 #f)
    => '("Makefile"))
  ;; (check
  ;;     (glibc.glob/string "Makefile*" 0 #f)
  ;;   => '("Makefile"))

  (when #f
    (check
	(glibc.glob/string "~marco" GLOB_TILDE #f)
      => '("/home/marco"))

    (let ((pwd (px.getcwd)))
      (px.chdir "/")
      (check-pretty-print (glibc.glob/string "*" 0 #f))
      (px.chdir pwd)))

;;; --------------------------------------------------------------------

  (check	;full match
      (let ((rex (glibc.regcomp "abc" 0)))
	(glibc.regexec rex "abc" 0))
    => '#((0 . 3)))

  (check	;partial match
      (let ((rex (glibc.regcomp "abc" 0)))
	(glibc.regexec rex "abcdef" 0))
    => '#((0 . 3)))

  (check	;no match
      (let ((rex (glibc.regcomp "ciao" 0)))
	(glibc.regexec rex "abc" 0))
    => #f)

  (check	;full bytevector match
      (let ((rex (glibc.regcomp '#vu8(65 66 67) 0)))
	(glibc.regexec rex '#vu8(65 66 67) 0))
    => '#((0 . 3)))

  (check
      (let ((rex (glibc.regcomp "\\(a\\)" 0)))
	(glibc.regexec rex "abc" 0))
    => '#((0 . 1)
	  (0 . 1)))

  (check
      (let ((rex (glibc.regcomp "\\(a\\)\\(b\\)\\(c\\)" 0)))
	(glibc.regexec rex "abc" 0))
    => '#((0 . 3)
	  (0 . 1)
	  (1 . 2)
	  (2 . 3)))

  (check
      (let ((rex (glibc.regcomp "\\(a\\(b\\(c\\)\\)\\)" 0)))
	(glibc.regexec rex "abc" 0))
    => '#((0 . 3)
	  (0 . 3)
	  (1 . 3)
	  (2 . 3)))

  (check	;releasing rex
      (let* ((rex (glibc.regcomp "abc" 0))
	     (rv  (glibc.regexec rex "abc" 0)))
	(glibc.regfree rex)
	(glibc.regfree rex) ;nothing happens
	rv)
    => '#((0 . 3)))

  (check
      (let* ((rex (glibc.regcomp "[a-z]+" REG_EXTENDED))
	     (rv  (glibc.regexec rex "abc" 0)))
	(glibc.regfree rex)
	rv)
    => '#((0 . 3)))

  (check
      (let* ((rex (glibc.regcomp/disown "[a-z]+" REG_EXTENDED))
	     (rv  (glibc.regexec rex "abc" 0)))
	(glibc.regfree rex)
	rv)
    => '#((0 . 3)))

;;;  (check-pretty-print 'gc)
  (collect))


(parametrise ((check-test-name	'word-expansion))

  (check
      (begin
	(px.setenv "CIAO" "BLU")
	(glibc.wordexp/string "$CIAO" 0))
    => '#("BLU"))

  (check
      (begin
	(px.setenv "CIAO" "BLUETTE")
	(glibc.wordexp/string "${CIAO##BLU}" 0))
    => '#("ETTE"))

  (when #t
    (check-pretty-print (glibc.wordexp/string "/bin/ch*" 0)))

  #t)


;;;; done

(check-report)

;;; end of file
