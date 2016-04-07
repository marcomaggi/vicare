;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: table of interned strings
;;;Date: Thu Sep 12, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(library (ikarus.strings-table)
  (export
    ;;These are only for internal use.
    $initialize-interned-strings-table!
    intern-string
    $interned-strings)
  (import (except (vicare)
		  intern-string)
    (vicare system $fx)
    (only (vicare system $hashtables)
	  $string-hash)
    (only (vicare system $strings)
	  $string-length
	  $string=))

  (define STRING-TABLE #f)

  (define ($initialize-interned-strings-table!)
    (set! STRING-TABLE (make-hashtable $string-hash $string=)))

  (define* (intern-string {str string?})
    ;;We do  not cache empty  strings, because sometimes an  empty string is  used as
    ;;small-memory-usage unique object.  For example:  the expander's marks are empty
    ;;strings.
    (if ($fxzero? ($string-length str))
	str
      (or (hashtable-ref STRING-TABLE str #f)
	  (begin
	    (hashtable-set! STRING-TABLE str str)
	    str))))

  (define ($interned-strings)
    (hashtable-keys STRING-TABLE))

  ;; (define end-of-file-dummy
  ;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.strings-table end")))

  #| end of library |# )

;;; end of file
