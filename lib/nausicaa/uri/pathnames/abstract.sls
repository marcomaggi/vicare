;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: file system pathname handling
;;;Date: Fri Mar 26, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2006-2007, 2010-2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa uri pathnames abstract)
  (export
    <pathname>
    <absolute-pathname>
    <relative-pathname>

    ;; multimethods
    pathname-absolute
    pathname-prepend		pathname-append
    pathname-string

    pathname-tail		pathname-dirname
    pathname-rootname		pathname-name
    pathname-extension		pathname-replace-extension)
  (import (nausicaa)
    (vicare unsafe operations))


;;;; multimethods

(define-generic pathname-absolute		(self absolute-pathname))
(define-generic pathname-prepend		(relative-self relative-pathname))
(define-generic pathname-append			(relative-self relative-pathname))
(define-generic pathname-string			(self))

(define-generic pathname-tail			(absolute-self))
(define-generic pathname-dirname		(absolute-self))
(define-generic pathname-rootname		(absolute-self))
(define-generic pathname-name			(absolute-self))
(define-generic pathname-extension		(absolute-self))
(define-generic pathname-replace-extension	(absolute-self source))


(define-class <pathname>
  (nongenerative nausicaa:uri:pathnames:abstract:<pathname>)
  (abstract)

  (super-protocol
   (lambda (make-top)
     (lambda ()
       ((make-top)))))

  (virtual-fields
   (immutable (bytevector	<bytevector>)	pathname-bytevector)
   (immutable (string		<string>)	pathname-string))

  (methods (absolute		pathname-absolute)
	   (append		pathname-append)
	   (dirname		pathname-dirname)
	   (rootname		pathname-rootname)
	   (tail		pathname-tail)
	   (extension		pathname-extension)
	   (name		pathname-name)
	   (replace-extension	pathname-replace-extension))

  #| end of class |# )


(define-class <absolute-pathname>
  (nongenerative nausicaa:uri:pathnames:abstract:<absolute-pathname>)
  (abstract)
  (parent <pathname>)

  (super-protocol
   (lambda (make-pathname)
     (lambda (bytevector segments)
       ((make-pathname)))))

  (methods (prepend	pathname-prepend))

  #| end of class |# )


(define-class <relative-pathname>
  (nongenerative nausicaa:uri:pathnames:abstract:<relative-pathname>)
  (abstract)
  (parent <pathname>)

  (super-protocol
   (lambda (make-pathname)
     (lambda (bytevector segments)
       ((make-pathname)))))

  (methods (append	pathname-append))

  #| end of class |# )


;;;; done

)

;;; end of file
