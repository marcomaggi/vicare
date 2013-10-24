;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: customisable type interface for generics library
;;;Date: Mon Jan 17, 2011
;;;
;;;Abstract
;;;
;;;	We need to export four bindings:
;;;
;;;	UID-LIST - A syntax which, applied to a type identifier, expands
;;;	into  a list  of symbols  representing the  type  hierarchy from
;;;	subclass to parent class.  All  the expansions of this macro for
;;;	the same  identifier must  yield the same  list in the  sense of
;;;	EQ?.
;;;
;;;	UID-LIST-OF - A function which,  applied to any value, returns a
;;;	list of symbols representing the type hierarchy from subclass to
;;;	parent  class.    The  returned  value  must  be   EQ?   to  the
;;;	corresponding expansion of UID-LIST.
;;;
;;;	METHOD-LAMBDA  -  A  syntax  which  must work  like  LAMBDA  but
;;;	recognise arguments tagged with types.
;;;
;;;	top - An identifier representing the topmost parent type for all
;;;	the classes.
;;;
;;;Copyright (C) 2011, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa language multimethods types)
  (export uid-list method-lambda top)
  (import (rename (only (nausicaa language oopp)
			tag-unique-identifiers
			lambda/tags
			<top>)
		  (tag-unique-identifiers	uid-list)
		  (lambda/tags			method-lambda)
		  (<top>			top))))

;;; end of file
;; Local Variables:
;; coding: utf-8
;; End:
