;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for Unix pathnames parser functions
;;;Date: Wed Nov 20, 2013
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


#!vicare
(import (vicare)
  (prefix (vicare parser-tools unix-pathnames) uxptn.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: Unix pathnames parser functions\n")


(parametrise ((check-test-name	'predicates))

  (check-for-true (uxptn.pathname? ""))
  (check-for-true (uxptn.pathname? "."))
  (check-for-true (uxptn.pathname? ".."))
  (check-for-true (uxptn.pathname? "/"))
  (check-for-true (uxptn.pathname? "ciao"))
  (check-for-true (uxptn.pathname? "ciao/hello"))

;;; --------------------------------------------------------------------

  (check-for-true (uxptn.pathname? '#ve(ascii "")))
  (check-for-true (uxptn.pathname? '#ve(ascii ".")))
  (check-for-true (uxptn.pathname? '#ve(ascii "..")))
  (check-for-true (uxptn.pathname? '#ve(ascii "/")))
  (check-for-true (uxptn.pathname? '#ve(ascii "ciao")))
  (check-for-true (uxptn.pathname? '#ve(ascii "ciao/hello")))

;;; --------------------------------------------------------------------

  (check-for-false (uxptn.pathname? "ciao\x1FF;"))
  (check-for-false (uxptn.pathname? "ciao\x0;"))

;;; --------------------------------------------------------------------

  (check-for-true (uxptn.segment? ""))
  (check-for-true (uxptn.segment? "."))
  (check-for-true (uxptn.segment? ".."))
  (check-for-false (uxptn.segment? "/"))
  (check-for-true (uxptn.segment? "ciao"))
  (check-for-false (uxptn.segment? "ciao/hello"))

;;; --------------------------------------------------------------------

  (check-for-true (uxptn.segment? '#ve(ascii "")))
  (check-for-true (uxptn.segment? '#ve(ascii ".")))
  (check-for-true (uxptn.segment? '#ve(ascii "..")))
  (check-for-false (uxptn.segment? '#ve(ascii "/")))
  (check-for-true (uxptn.segment? '#ve(ascii "ciao")))
  (check-for-false (uxptn.segment? '#ve(ascii "ciao/hello")))

;;; --------------------------------------------------------------------

  (check-for-false (uxptn.segment? "ciao\x1FF;"))
  (check-for-false (uxptn.segment? "ciao\x0;"))

;;; --------------------------------------------------------------------

  (check-for-true (uxptn.list-of-segments? '("")))
  (check-for-true (uxptn.list-of-segments? '(".")))
  (check-for-true (uxptn.list-of-segments? '("..")))
  (check-for-false (uxptn.list-of-segments? '("/")))
  (check-for-true (uxptn.list-of-segments? '("ciao")))
  (check-for-false (uxptn.list-of-segments? '("ciao/hello")))
  (check-for-true (uxptn.list-of-segments? '("ciao" "hello")))

;;; --------------------------------------------------------------------

  (check-for-true (uxptn.list-of-segments? '(#ve(ascii ""))))
  (check-for-true (uxptn.list-of-segments? '(#ve(ascii "."))))
  (check-for-true (uxptn.list-of-segments? '(#ve(ascii ".."))))
  (check-for-false (uxptn.list-of-segments? '(#ve(ascii "/"))))
  (check-for-true (uxptn.list-of-segments? '(#ve(ascii "ciao"))))
  (check-for-false (uxptn.list-of-segments? '(#ve(ascii "ciao/hello"))))
  (check-for-true (uxptn.list-of-segments? '(#ve(ascii "ciao") #ve(ascii "hello"))))

;;; --------------------------------------------------------------------

  (check-for-false (uxptn.list-of-segments? '("ciao\x1FF;")))
  (check-for-false (uxptn.list-of-segments? '("ciao\x0;")))

  #t)


(parametrise ((check-test-name	'conversion))

  (check
      (uxptn.string/bytevector->pathname-bytevector "")
    => '#ve(ascii "."))

  (check
      (uxptn.string/bytevector->pathname-bytevector ".")
    => '#ve(ascii "."))

  (check
      (uxptn.string/bytevector->pathname-bytevector "..")
    => '#ve(ascii ".."))

  (check
      (uxptn.string/bytevector->pathname-bytevector "/")
    => '#ve(ascii "/"))

  (check
      (uxptn.string/bytevector->pathname-bytevector "ciao")
    => '#ve(ascii "ciao"))

  (check
      (uxptn.string/bytevector->pathname-bytevector "ciao/hello")
    => '#ve(ascii "ciao/hello"))

;;; --------------------------------------------------------------------

  (check
      (uxptn.string/bytevector->pathname-bytevector '#ve(ascii ""))
    => '#ve(ascii "."))

  (check
      (uxptn.string/bytevector->pathname-bytevector '#ve(ascii "."))
    => '#ve(ascii "."))

  (check
      (uxptn.string/bytevector->pathname-bytevector '#ve(ascii ".."))
    => '#ve(ascii ".."))

  (check
      (uxptn.string/bytevector->pathname-bytevector '#ve(ascii "/"))
    => '#ve(ascii "/"))

  (check
      (uxptn.string/bytevector->pathname-bytevector '#ve(ascii "ciao"))
    => '#ve(ascii "ciao"))

  (check
      (uxptn.string/bytevector->pathname-bytevector '#ve(ascii "ciao/hello"))
    => '#ve(ascii "ciao/hello"))

;;; --------------------------------------------------------------------

  (check
      (uxptn.pathname-bytevector->string (uxptn.string/bytevector->pathname-bytevector "ciao"))
    => "ciao")

  (check
      (uxptn.pathname-bytevector->string (uxptn.string/bytevector->pathname-bytevector "ciao/hello"))
    => "ciao/hello")

;;; --------------------------------------------------------------------

  (check
      (guard (E ((uxptn.unix-pathname-parser-error? E)
		 #t)
		(else E))
	(uxptn.string/bytevector->pathname-bytevector "ciao\x1FF;"))
    => #t)

  (check
      (guard (E ((uxptn.unix-pathname-parser-error? E)
		 #t)
		(else E))
	(uxptn.string/bytevector->pathname-bytevector "ciao\x0;"))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
