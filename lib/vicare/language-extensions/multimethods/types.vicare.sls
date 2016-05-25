;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Scheme
;;;Contents: customisable type interface for generics library
;;;Date: Mon Jan 17, 2011
;;;
;;;Abstract
;;;
;;;   We need to export four bindings:
;;;
;;;   TYPE-UNIQUE-IDENTIFIERS - A syntax which, applied to a type identifier, expands
;;;   into a list of symbols representing  the type hierarchy from subclass to parent
;;;   class.  All the expansions of this macro for the same identifier must yield the
;;;   same list in the sense of EQ?.
;;;
;;;      (type-unique-identifiers <procedure>)
;;;      ==> (vicare:scheme-type:<procedure> vicare:scheme-type:<top>)
;;;
;;;   METHOD-LAMBDA -  A syntax which must  work like LAMBDA but  recognise arguments
;;;   tagged with types.
;;;
;;;   <top> - An identifier representing the topmost parent type for all the classes.
;;;
;;;Copyright (C) 2011-2013, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare language-extensions multimethods types (0 4 2016 5 24))
  (export type-unique-identifiers method-lambda <top>)
  (import (rename (only (vicare)
			type-unique-identifiers
			named-lambda/typed
			<top>)
		  (named-lambda/typed	method-lambda))))

;;; end of file
