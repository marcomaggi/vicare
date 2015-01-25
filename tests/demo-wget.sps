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
  (vicare posix wget))

(define (get-it url)
  (receive (out err)
      (wget "--output-document=-" url)
    (display out)
    (newline)
    (display err)
    (newline)
    (flush-output-port (current-output-port))))

(get-it "http://marcomaggi.github.io/index.html")

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
