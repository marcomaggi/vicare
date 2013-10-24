;;;
;;;Part of: Vicare Scheme
;;;Contents: implementation of SRFI 112, environment inquiry
;;;Date: Sun Sep 15, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (ikarus.environment-inquiry)
  (export
    uname
    utsname?
    utsname-sysname
    utsname-nodename
    utsname-release
    utsname-version
    utsname-machine

    implementation-name
    implementation-version
    cpu-architecture
    machine-name
    os-name
    os-version)
  (import (except (vicare)
		  uname
		  utsname?
		  utsname-sysname
		  utsname-nodename
		  utsname-release
		  utsname-version
		  utsname-machine

		  implementation-name
		  implementation-version
		  cpu-architecture
		  machine-name
		  os-name
		  os-version

		  host-info))

  (include "ikarus.config.ss")


;;;; implementation

(define-struct utsname
  (sysname nodename release version machine))

(module ()
  (set-rtd-printer! (type-descriptor utsname)
    (lambda (S port unused)
      (define-inline (%display thing)
	(display thing port))
      (define-inline (%write thing)
	(write thing port))
      (%display "#[utsname")
      (%display " sysname=")	(%write ($utsname-sysname  S))
      (%display " nodename=")	(%write ($utsname-nodename S))
      (%display " release=")	(%write ($utsname-release  S))
      (%display " version=")	(%write ($utsname-version  S))
      (%display " machine=")	(%write ($utsname-machine  S))
      (%display "]"))))

(define (uname)
  (let* ((stru (make-utsname #f #f #f #f #f))
	 (rv   (foreign-call "ikrt_posix_uname" stru)))
    (if rv
	(raise (condition
		(make-error)
		(make-errno-condition rv)
		(make-who-condition 'uname)
		(make-message-condition (strerror errno))))
      stru)))

;;; --------------------------------------------------------------------

(define (implementation-name)
  "vicare-scheme")

(define (implementation-version)
  vicare-version)

(define (cpu-architecture)
  (utsname-machine (uname)))

(define (machine-name)
  (utsname-nodename (uname)))

(define (os-name)
  (utsname-sysname (uname)))

(define (os-version)
  (utsname-version (uname)))


;;;; done

)

;;; end of file
