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
;;;Copyright (c) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (rename (vicare)
		(parameterize	parametrise))
  (prefix (vicare glibc)
	  glibc.)
  (vicare platform-constants)
  (vicare syntactic-extensions)
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare GNU C Library functions\n")


(parametrise ((check-test-name	'directories))

  (check
      (let* ((stream (opendir ".."))
	     (fd     (glibc.dirfd stream)))
	(closedir stream)
	(fixnum? fd))
    => #t)

  (let ((pwd (getcwd/string)))
    (check
	(begin
	  (mkdir "one" S_IRWXU)
	  (unwind-protect
	      (let ((stream (opendir "one")))
		(unwind-protect
		    (let ((fd (glibc.dirfd stream)))
		      (fchdir fd)
		      (getcwd/string))
		  (closedir stream)))
	    (chdir pwd)
	    (rmdir "one")))
      => (string-append pwd "/one")))

  #t)


(parametrise ((check-test-name	'temporary))

  (check
      (let* ((template	((string->filename-func) "/tmp/tmp.XXXXXX"))
	     (fd	(glibc.mkstemp template)))
;;;	(check-pretty-print ((filename->string-func) template))
	(unwind-protect
	    (fixnum? fd)
	  #f))
    => #t)

  (check
      (let* ((template	((string->filename-func) "/tmp/tmp.XXXXXX"))
	     (name	(glibc.mkdtemp template)))
;;;	(check-pretty-print ((filename->string-func) template))
;;;	(check-pretty-print ((filename->string-func) name))
	(unwind-protect
	    (bytevector? name)
	  #f))
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
  (check (glibc.if-indextoname 2)	=> "eth0")

  (check (glibc.if-nametoindex "lo")		=> 1)
  (check (glibc.if-nametoindex "eth0")	=> 2)

  (check-pretty-print (list 'alist-of-ifaces (glibc.if-nameindex)))

;;; --------------------------------------------------------------------

  #t)


(parametrise ((check-test-name	'math))

  (check (cflonum? (glibc.csin 1.2+3.4i))	=> #t)
  (check (cflonum? (glibc.ccos 1.2+3.4i))	=> #t)
  (check (cflonum? (glibc.ctan 1.2+3.4i))	=> #t)
  (check (cflonum? (glibc.casin 1.2+3.4i))	=> #t)
  (check (cflonum? (glibc.cacos 1.2+3.4i))	=> #t)
  (check (cflonum? (glibc.catan 1.2+3.4i))	=> #t)

  (check (cflonum? (glibc.cexp 1.2+3.4i))	=> #t)
  (check (cflonum? (glibc.clog 1.2+3.4i))	=> #t)
  (check (cflonum? (glibc.clog10 1.2+3.4i))	=> #t)
  (check (cflonum? (glibc.csqrt 1.2+3.4i))	=> #t)
  (check (cflonum? (glibc.cpow 1.2+3.4i 5.6+7.8i))	=> #t)

  (check (flonum? (glibc.glibc-sinh 1.2))	=> #t)
  (check (flonum? (glibc.glibc-cosh 1.2))	=> #t)
  (check (flonum? (glibc.glibc-tanh 1.2))	=> #t)
  (check (flonum? (glibc.glibc-asinh 1.2))	=> #t)
  (check (flonum? (glibc.glibc-acosh 1.2))	=> #t)
  (check (flonum? (glibc.glibc-atanh 1.2))	=> #t)
  (check (cflonum? (glibc.csinh 1.2+3.4i))	=> #t)
  (check (cflonum? (glibc.ccosh 1.2+3.4i))	=> #t)
  (check (cflonum? (glibc.ctanh 1.2+3.4i))	=> #t)
  (check (cflonum? (glibc.casinh 1.2+3.4i))	=> #t)
  (check (cflonum? (glibc.cacosh 1.2+3.4i))	=> #t)
  (check (cflonum? (glibc.catanh 1.2+3.4i))	=> #t)

  #t)


;;;; done

(check-report)

;;; end of file
