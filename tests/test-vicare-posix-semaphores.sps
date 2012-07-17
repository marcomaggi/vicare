;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for (vicare posix) semaphores
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
  (vicare platform-constants)
  (vicare syntactic-extensions)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing POSIX library, semaphores\n")


(parametrise ((check-test-name	'semaphores))

;;; named semaphores

  (check	;open and close
      (let ((sem.pathname "/vicare-posix-sem.test"))
	(guard (E (else #f))
	  (px.sem-unlink sem.pathname))
	(let ((sem_t (callet px.sem-open sem.pathname
			     (oflags	(fxior O_CREAT O_EXCL O_RDWR))
			     (mode	(fxior S_IRUSR S_IWUSR)))))
	  (unwind-protect
	      (unwind-protect
		  (pointer? sem_t)
		(px.sem-close sem_t))
	    (px.sem-unlink sem.pathname))))
    => #t)

  (check	;multiple processes timed post and timed wait
      (let ((sem.pathname "/vicare-posix-sem.test")
	    (shm.pathname "/vicare-posix-shm.test")
	    (shm.dim      (px.sysconf _SC_PAGESIZE)))
	(define (parent child-pid)
	  (let ((sem_t  (callet px.sem-open sem.pathname
				(oflags	(fxior O_CREAT O_EXCL O_RDWR))
				(mode	(fxior S_IRUSR S_IWUSR))))
		(shm.fd (callet px.shm-open shm.pathname
				(oflags   (fxior O_CREAT O_EXCL O_RDWR))
				(mode     (fxior S_IRUSR S_IWUSR)))))
	    (px.ftruncate shm.fd shm.dim)
	    (unwind-protect
		(let ((shm.base (callet px.mmap
					(address #f)
					(size    shm.dim)
					(prot    (fxior PROT_READ PROT_WRITE))
					(flags   MAP_SHARED)
					(fd      shm.fd)
					(offset  0))))
		  (unwind-protect
		      (begin
			(px.sem-wait sem_t)
			(pointer-ref-c-signed-int shm.base 0))
		    (px.munmap shm.base shm.dim)))
	      (px.close shm.fd)
	      (px.shm-unlink shm.pathname)
	      (px.sem-close sem_t)
	      (px.sem-unlink sem.pathname))))
	(define (child)
	  (px.nanosleep 1 0)
	  (let ((sem_t  (callet px.sem-open sem.pathname
				(oflags	(fxior O_CREAT O_RDWR))
				(mode	(fxior S_IRUSR S_IWUSR))))
		(shm.fd (callet px.shm-open shm.pathname
				(oflags (fxior O_CREAT O_RDWR))
				(mode   (fxior S_IRUSR S_IWUSR)))))
	    (unwind-protect
		(let ((shm.base (callet px.mmap
					(address #f)
					(size    shm.dim)
					(prot    (fxior PROT_READ PROT_WRITE))
					(flags   MAP_SHARED)
					(fd      shm.fd)
					(offset  0))))
		  (unwind-protect
		      (begin
			(pointer-set-c-signed-int! shm.base 0 123)
			(px.sem-post sem_t))
		    (px.munmap shm.base shm.dim)))
	      (px.close shm.fd)
	      (px.sem-close sem_t)))
	  (exit 0))
	(guard (E (else #f))
	  (px.sem-unlink sem.pathname))
	(guard (E (else #f))
	  (px.shm-unlink shm.pathname))
	(px.fork parent child))
    => 123)

;;; --------------------------------------------------------------------
;;; unnamed semaphores

  (check	;alloc and release in normal memory
      (let* ((sem_t (malloc (px.sizeof-sem_t)))
	     (sem_t (callet px.sem-init	sem_t
			    (pshared?	#f))))
	(unwind-protect
	    (pointer? sem_t)
	  (px.sem-destroy sem_t)))
    => #t)

  (check	;alloc and release in POSIX shared memory
      (let ((shm.pathname "/vicare-posix-sem-shm.test")
	    (shm.dim      (px.sysconf _SC_PAGESIZE)))
	(guard (E (else #f))
	  (px.shm-unlink shm.pathname))
	(let ((shm.fd (callet px.shm-open shm.pathname
			      (oflags   (fxior O_CREAT O_EXCL O_RDWR))
			      (mode     (fxior S_IRUSR S_IWUSR)))))
	  (px.ftruncate shm.fd shm.dim)
	  (unwind-protect
	      (let ((shm.base (callet px.mmap
				      (address #f)
				      (size    shm.dim)
				      (prot    (fxior PROT_READ PROT_WRITE))
				      (flags   MAP_SHARED)
				      (fd      shm.fd)
				      (offset  0))))
		(unwind-protect
		    (let* ((sem_t	shm.base)
			   (shm.start	(pointer-add shm.base (px.sizeof-sem_t)))
			   (sem_t	(callet px.sem-init sem_t
						(pshared? #t)
						(value    0))))
		      (unwind-protect
			  (pointer? sem_t)
			(px.sem-destroy sem_t)))
		  (px.munmap shm.base shm.dim)))
	    (px.close shm.fd)
	    (px.shm-unlink shm.pathname))))
    => #t)

  (check	;multiple processes post and wait
      (let ((shm.pathname "/vicare-posix-sem-shm.test")
	    (shm.dim      (px.sysconf _SC_PAGESIZE)))
	(define (parent child-pid)
	  (let ((shm.fd (callet px.shm-open shm.pathname
				(oflags   (fxior O_CREAT O_EXCL O_RDWR))
				(mode     (fxior S_IRUSR S_IWUSR)))))
	    (px.ftruncate shm.fd shm.dim)
	    (unwind-protect
		(let ((shm.base (callet px.mmap
					(address #f)
					(size    shm.dim)
					(prot    (fxior PROT_READ PROT_WRITE))
					(flags   MAP_SHARED)
					(fd      shm.fd)
					(offset  0))))
		  (unwind-protect
		      (let* ((sem_t	shm.base)
			     (shm.start	(pointer-add shm.base (px.sizeof-sem_t)))
			     (sem_t	(callet px.sem-init sem_t
						(pshared? #t)
						(value    0))))
			(unwind-protect
			    (begin
			      (px.sem-wait sem_t)
			      (pointer-ref-c-signed-int shm.start 0))
			  (px.sem-destroy sem_t)))
		    (px.munmap shm.base shm.dim)))
	      (px.close shm.fd)
	      (px.shm-unlink shm.pathname))))
	(define (child)
	  (px.nanosleep 1 0)
	  (let ((shm.fd (callet px.shm-open shm.pathname
				(oflags   (fxior O_CREAT O_RDWR))
				(mode     (fxior S_IRUSR S_IWUSR)))))
	    (unwind-protect
		(let ((shm.base (callet px.mmap
					(address #f)
					(size    shm.dim)
					(prot    (fxior PROT_READ PROT_WRITE))
					(flags   MAP_SHARED)
					(fd      shm.fd)
					(offset  0))))
		  (unwind-protect
		      (let* ((sem_t	shm.base)
			     (shm.start	(pointer-add shm.base (px.sizeof-sem_t))))
			(pointer-set-c-signed-int! shm.start 0 123)
			(px.sem-post sem_t))
		    (px.munmap shm.base shm.dim)))
	      (px.close shm.fd)))
	  (exit 0))
	(guard (E (else #f))
	  (px.shm-unlink shm.pathname))
	(px.fork parent child))
    => 123)

  (check	;multiple processes timed post and timed wait
      (let ((shm.pathname "/vicare-posix-sem-shm.test")
	    (shm.dim      (px.sysconf _SC_PAGESIZE)))
	(define (parent child-pid)
	  (let ((shm.fd (callet px.shm-open shm.pathname
				(oflags   (fxior O_CREAT O_EXCL O_RDWR))
				(mode     (fxior S_IRUSR S_IWUSR)))))
	    (px.ftruncate shm.fd shm.dim)
	    (unwind-protect
		(let ((shm.base (callet px.mmap
					(address #f)
					(size    shm.dim)
					(prot    (fxior PROT_READ PROT_WRITE))
					(flags   MAP_SHARED)
					(fd      shm.fd)
					(offset  0))))
		  (unwind-protect
		      (let* ((sem_t	shm.base)
			     (shm.start	(pointer-add shm.base (px.sizeof-sem_t)))
			     (sem_t	(callet px.sem-init sem_t
						(pshared? #t)
						(value    0))))
			(define timeout
			  (let ((T (px.clock-gettime CLOCK_REALTIME
				     (px.make-struct-timespec 0 0))))
			    (px.set-struct-timespec-tv_sec! T
			      (+ 2 (px.struct-timespec-tv_sec T)))
			    T))
			(unwind-protect
			    (begin
			      (px.sem-timedwait sem_t timeout)
			      (pointer-ref-c-signed-int shm.start 0))
			  (px.sem-destroy sem_t)))
		    (px.munmap shm.base shm.dim)))
	      (px.close shm.fd)
	      (px.shm-unlink shm.pathname))))
	(define (child)
	  (px.nanosleep 1 0)
	  (let ((shm.fd (callet px.shm-open shm.pathname
				(oflags   (fxior O_CREAT O_RDWR))
				(mode     (fxior S_IRUSR S_IWUSR)))))
	    (unwind-protect
		(let ((shm.base (callet px.mmap
					(address #f)
					(size    shm.dim)
					(prot    (fxior PROT_READ PROT_WRITE))
					(flags   MAP_SHARED)
					(fd      shm.fd)
					(offset  0))))
		  (unwind-protect
		      (let* ((sem_t	shm.base)
			     (shm.start	(pointer-add shm.base (px.sizeof-sem_t))))
			(pointer-set-c-signed-int! shm.start 0 123)
			(px.sem-post sem_t))
		    (px.munmap shm.base shm.dim)))
	      (px.close shm.fd)))
	  (exit 0))
	(guard (E (else #f))
	  (px.shm-unlink shm.pathname))
	(px.fork parent child))
    => 123)

  #t)


;;;; done

(check-report)

;;; end of file
