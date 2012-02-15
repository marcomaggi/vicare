;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for shared memory among processes
;;;Date: Wed Feb 15, 2012
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
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (prefix (vicare posix) px.)
;;;(prefix (vicare glibc) glibc.)
  (vicare platform-constants)
  (vicare syntactic-extensions)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing Vicare mmapped shared memory\n")


(parametrise ((check-test-name	'raw))

  (check		     ;exchanging integer
      (let* ((shmem.len	4096 #;(glibc.sysconf _SC_PAGESIZE))
	     (shmem.ptr	(px.mmap #f shmem.len
				 (fxior PROT_READ PROT_WRITE)
				 (fxior MAP_SHARED MAP_ANONYMOUS)
				 0 0)))
	(unwind-protect
	    (px.fork (lambda (child-pid) ;parent
		       (pointer-set-c-signed-int! shmem.ptr 0 234)
		       (px.waitpid child-pid 0)
		       (pointer-ref-c-signed-int shmem.ptr 0))
		     (lambda () ;child
		       (px.nanosleep 1 0)
		       (pointer-set-c-signed-int! shmem.ptr 0
						  (+ 1000 (pointer-ref-c-signed-int shmem.ptr 0)))
		       (exit)))
	  (px.munmap shmem.ptr shmem.len)))
    => 1234)

  (check		     ;exchanging bytevector
      (let* ((shmem.len	4096 #;(glibc.sysconf _SC_PAGESIZE))
	     (shmem.ptr	(px.mmap #f shmem.len
				 (fxior PROT_READ PROT_WRITE)
				 (fxior MAP_SHARED MAP_ANONYMOUS)
				 0 0)))
	(unwind-protect
	    (px.fork (lambda (child-pid) ;parent
		       (memory-copy shmem.ptr 0 '#vu8(0 1 2 3 4 5 6 7 8 9) 0 10)
		       (px.waitpid child-pid 0)
		       (let ((bv (make-bytevector 10)))
			 (memory-copy bv 0 shmem.ptr 0 10)
			 bv))
		     (lambda () ;child
		       (px.nanosleep 1 0)
		       (let ((bv (make-bytevector 10)))
			 (memory-copy bv 0 shmem.ptr 0 10)
			 (do ((i 0 (+ 1 i)))
			     ((= i 10)
			      (memory-copy shmem.ptr 0 bv 0 10)
			      (exit))
			   (bytevector-u8-set! bv i (+ 100 (bytevector-u8-ref bv i)))))))
	  (px.munmap shmem.ptr shmem.len)))
    => '#vu8(100 101 102 103 104 105 106 107 108 109))

  #t)


(parametrise ((check-test-name	'fasl))

  (let ((data '(1 2 #(3 4) ciao "hello" #vu8(99 98 97))))
    (check	;exchanging bytevector
	(let* ((shmem.len (* 16 4096 #;(glibc.sysconf _SC_PAGESIZE)))
	       (shmem.ptr (px.mmap #f shmem.len
				   (fxior PROT_READ PROT_WRITE)
				   (fxior MAP_SHARED MAP_ANONYMOUS)
				   0 0)))
	  (unwind-protect
	      (px.fork (lambda (child-pid) ;parent
			 (px.waitpid child-pid 0)
			 (let* ((bv.len (pointer-ref-c-signed-int shmem.ptr 0))
				(bv     (make-bytevector bv.len)))
			   (memory-copy bv 0 shmem.ptr SIZEOF_INT bv.len)
			   (let ((port (open-bytevector-input-port bv)))
			     (fasl-read port))))
		       (lambda () ;child
			 (let-values (((port getter) (open-bytevector-output-port)))
			   (fasl-write data port)
			   (let* ((bv     (getter))
				  (bv.len (bytevector-length bv)))
			     (pointer-set-c-signed-int! shmem.ptr 0 bv.len)
			     (memory-copy shmem.ptr SIZEOF_INT bv 0 bv.len)
			     (exit)))))
	    (px.munmap shmem.ptr shmem.len)))
      => data))

  #t)


;;;; done

(check-report)

;;; end of file
