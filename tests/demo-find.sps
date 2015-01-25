;;;
;;;Part of: Vicare Scheme
;;;Contents: demo program to download files with wget
;;;Date: Sun Jan 25, 2015
;;;
;;;Abstract
;;;
;;;	Demonstration of how to use the library (vicare posix wget) to download files
;;;	from the Net using the external program "wget".
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (vicare)
  (prefix (vicare posix) px.)
  (prefix (vicare posix find) find.)
  (only (vicare containers strings)
	string-tokenise)
  (only (vicare containers char-sets)
	char-set char-set-complement))

(define (scan-it root)
  (receive (status out err)
      (find.find root "-type" "f"
		 "-and" "-executable"
		 "-and" "-maxdepth" "2"
		 "-and" "-iname" "*bin*"
		 "-and" "-print0")
    (when (string? out)
      (for-each (lambda (hit)
		  (write hit)
		  (newline))
	(string-tokenise out (char-set-complement (char-set #\x00)))))
    (display err)
    (newline)
    (flush-output-port (current-output-port))))

(scan-it "/usr")

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
