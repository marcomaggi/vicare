;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for (vicare posix) shared memory
;;;Date: Tue Jul 17, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(import (vicare)
  (prefix (vicare posix)
	  px.)
  (vicare platform constants)
  (vicare language-extensions syntaxes)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing POSIX library, shared memory\n")


(parametrise ((check-test-name	'shared-memory))

  (check	;open and close
      (let ()
	(define shm.pathname "/vicare-posix-shm.test")
	(define shm.dim (px.sysconf _SC_PAGESIZE))
	(guard (E (else #f))
	  (px.shm-unlink shm.pathname))
	(let ((shm.fd (px.shm-open shm.pathname
				   (fxior O_CREAT O_EXCL O_RDWR)
				   (fxior S_IRUSR S_IWUSR))))
	  (unwind-protect
	      (unwind-protect
		  (let ((shm.base (px.mmap #f shm.dim
					   (fxior PROT_READ PROT_WRITE)
					   (fxior MAP_PRIVATE MAP_ANONYMOUS)
					   shm.fd 0)))
		    (unwind-protect
			(pointer? shm.base)
		      (px.munmap shm.base shm.dim)))
		(px.close shm.fd))
	    (px.shm-unlink shm.pathname))))
    => #t)

  (check	;exchange data between processes
      (let ()
	(define shm.pathname "/vicare-posix-shm.test")
	(define shm.dim (px.sysconf _SC_PAGESIZE))
	(define (parent child-pid)
	  (let ((shm.fd (callet px.shm-open shm.pathname
				(oflags (fxior O_CREAT O_RDWR))
				(mode	(fxior S_IRUSR S_IWUSR)))))
	    (px.ftruncate shm.fd shm.dim)
	    (unwind-protect
		(unwind-protect
		    (let ((shm.base (callet px.mmap
					    (address	#f)
					    (size	shm.dim)
					    (prot	(fxior PROT_READ PROT_WRITE))
					    (flags	MAP_SHARED)
					    (fd		shm.fd)
					    (offset	0))))
		      (unwind-protect
			  (begin
			    (guard (E (else #f))
			      (px.waitpid child-pid 0))
			    (pointer-ref-c-signed-int shm.base 0))
			(px.munmap shm.base shm.dim)))
		  (px.close shm.fd))
	      (px.shm-unlink shm.pathname))))
	(define (child)
	  (px.nanosleep 0 900000)
	  (let ((shm.fd (callet px.shm-open shm.pathname
				(oflags (fxior O_CREAT O_RDWR))
				(mode   (fxior S_IRUSR S_IWUSR)))))
	    (unwind-protect
		(let ((shm.base (callet px.mmap
					(address	#f)
					(size		shm.dim)
					(prot		(fxior PROT_READ PROT_WRITE))
					(flags		MAP_SHARED)
					(fd		shm.fd)
					(offset		0))))
		  (unwind-protect
		      (pointer-set-c-signed-int! shm.base 0 123)
		    (px.munmap shm.base shm.dim)))
	      (px.close shm.fd)))
	  (exit 0))
	(guard (E (else #f))
	  (px.shm-unlink shm.pathname))
	(px.fork parent child))
    => 123)

  #f)


;;;; done

(check-report)

;;; end of file
