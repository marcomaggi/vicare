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
    pathname=?

    pathname-bytevector
    pathname-string
    pathname-uri-representation

    pathname-extension
    pathname-dirname
    pathname-tailname
    pathname-rootname
    pathname-strip-trailing-slashes
    pathname-split
    pathname-normalise
    pathname-prefix?
    pathname-suffix?
    pathname-prepend
    pathname-append
    pathname-replace-extension)
  (import (nausicaa)
    (vicare unsafe operations))


;;;; multimethods

(define-generic pathname=?			(A B))

(define-generic pathname-bytevector		(self))
(define-generic pathname-string			(self))
(define-generic pathname-uri-representation	(self))

(define-generic pathname-extension		(self))
(define-generic pathname-dirname		(self))
(define-generic pathname-tailname		(self))
(define-generic pathname-rootname		(self))
(define-generic pathname-strip-trailing-slashes	(self))
(define-generic pathname-split			(self))
(define-generic pathname-normalise		(self))
(define-generic pathname-prefix?		(self other))
(define-generic pathname-suffix?		(self other))
(define-generic pathname-prepend		(self other))
(define-generic pathname-append			(self other))
(define-generic pathname-replace-extension	(self extension))


(define-class <pathname>
  (nongenerative nausicaa:uri:pathnames:abstract:<pathname>)
  (abstract)

  (super-protocol
   (lambda (make-top)
     (lambda ()
       ((make-top)))))

  (virtual-fields
   (immutable (bytevector	<bytevector>)	pathname-bytevector)
   (immutable (string		<string>)	pathname-string)
   (immutable (uri		<bytevector>)	pathname-uri-representation))

  (methods
   (extension			pathname-extension)
   (dirname			pathname-dirname)
   (tailname			pathname-tailname)
   (rootname			pathname-rootname)
   (strip-trailing-slashes	pathname-strip-trailing-slashes)
   (split			pathname-split)
   (normalise			pathname-normalise)
   (prefix?			pathname-prefix?)
   (suffix?			pathname-suffix?)
   (prepend			pathname-prepend)
   (replace-extension		pathname-replace-extension)

   #| end of methods |# )

  #| end of class |# )


(define-class <absolute-pathname>
  (nongenerative nausicaa:uri:pathnames:abstract:<absolute-pathname>)
  (abstract)
  (parent <pathname>)

  (super-protocol
   (lambda (make-pathname)
     (lambda ()
       ((make-pathname)))))

  #| end of class |# )

(define-class <relative-pathname>
  (nongenerative nausicaa:uri:pathnames:abstract:<relative-pathname>)
  (abstract)
  (parent <pathname>)

  (super-protocol
   (lambda (make-pathname)
     (lambda ()
       ((make-pathname)))))

  (methods
   (append			pathname-append)
   #| end of methods |# )

  #| end of class |# )


;;;; multimethods implementation

(define-method (pathname=? (A <absolute-pathname>) (B <relative-pathname>))
  #f)

(define-method (pathname=? (A <relative-pathname>) (B <absolute-pathname>))
  #f)


;;;; done

)

;;; end of file
