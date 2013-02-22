;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: test implementation of Iconv functions
;;;Date: Fri Feb 22, 2013
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
  (prefix (vicare iconv)
	  iconv.)
  (vicare platform constants)
  (vicare syntactic-extensions)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: iconv functions\n")


(parametrise ((check-test-name	'iconv))

;;; handle allocation

  (check
      (iconv.iconv? (iconv.iconv-open (iconv.iconv-encoding UTF-16)
				      (iconv.iconv-encoding UTF-8)))
    => #t)

  (check
      (iconv.iconv? (iconv.iconv-open (iconv.iconv-encoding UTF-16 TRANSLIT IGNORE)
				      (iconv.iconv-encoding UTF-8)))
    => #t)

  (check
      (iconv.iconv-closed? (iconv.iconv-open (iconv.iconv-encoding UTF-16)
					     (iconv.iconv-encoding UTF-8)))
    => #f)

  (check	;nothing happens if we close the handle multiple times
      (let ((handle (iconv.iconv-open (iconv.iconv-encoding UTF-16)
				      (iconv.iconv-encoding UTF-8))))
;;;(check-pretty-print handle)
	(iconv.iconv-close handle)
	(iconv.iconv-close handle)
	(iconv.iconv-close handle)
	(iconv.iconv-closed? handle))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (iconv.iconv-encoding-aliases? (iconv.iconv-encoding IBM819)
				     (iconv.iconv-encoding ISO-8859-1))
    => #t)

  (check
      (iconv.iconv-encoding-aliases? (iconv.iconv-encoding IBM819)
				     (iconv.iconv-encoding UTF-8))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (iconv.iconv-encoding=? (iconv.iconv-encoding IBM819)
			      (iconv.iconv-encoding ISO-8859-1))
    => #t)

  (check
      (iconv.iconv-encoding=? (iconv.iconv-encoding IBM819 TRANSLIT)
			      (iconv.iconv-encoding ISO-8859-1))
    => #f)

  (check
      (iconv.iconv-encoding=? (iconv.iconv-encoding IBM819 IGNORE)
			      (iconv.iconv-encoding ISO-8859-1))
    => #f)

  (check
      (iconv.iconv-encoding=? (iconv.iconv-encoding IBM819 TRANSLIT)
			      (iconv.iconv-encoding ISO-8859-1 TRANSLIT))
    => #t)

  (check
      (iconv.iconv-encoding=? (iconv.iconv-encoding IBM819 TRANSLIT)
			      (iconv.iconv-encoding ISO-8859-1 TRANSLIT IGNORE))
    => #f)

  (check
      (iconv.iconv-encoding=? (iconv.iconv-encoding IBM819)
			      (iconv.iconv-encoding UTF-8))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((H		(iconv.iconv-open (iconv.iconv-encoding UTF-16BE) ;from
					  (iconv.iconv-encoding UTF-8))) ;to
;;;                      0123456789012345
;;;                          012345678901
	     (in.str	"ciao hello salut")
	     (in.bv	(string->utf16 in.str (endianness big)))
	     (out.bv	(string->utf8 in.str))
	     (out.bv1	(make-bytevector 4 0))
	     (out.bv2	(make-bytevector (- (bytevector-length out.bv) 4) 0)))
	(let-values (((in.start1 out.start1) (iconv.iconv! H in.bv 0 #f out.bv1 0 #f)))
	  (let-values (((in.start2 out.start2) (iconv.iconv! H in.bv in.start1 #f out.bv2 0 #f)))
	    (list out.start1 out.start2
		  (= in.start2 (bytevector-length in.bv))
		  (bytevector=? out.bv (bytevector-append out.bv1 out.bv2))))))
    => '(4 12 #t #t))

  (check	;example to be used in the documentation
      (let* ((handle	(iconv.iconv-open (iconv.iconv-encoding UTF-16BE) ;from
					  (iconv.iconv-encoding UTF-8))) ;to
;;;                                     0123456789012345
	     (in.bv	(string->utf16 "ciao hello salut" (endianness big)))
	     (out.bv	(make-bytevector 16)))
	(let-values (((in.start out.start) (iconv.iconv! handle in.bv 0 #f out.bv 0 #f)))
	  (utf8->string out.bv)))
    => "ciao hello salut")

  #t)


;;;; done

(check-report)

;;; end of file
