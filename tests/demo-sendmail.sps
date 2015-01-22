;;;
;;;Part of: Vicare Scheme
;;;Contents: demo program to send mail with sendmail
;;;Date: Thu Jan 22, 2015
;;;
;;;Abstract
;;;
;;;
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
  (vicare posix sendmail))

(define message-template
  "From: <marco@localhost>
To: <marco@localhost>
Subject: demo ~a
MIME-Version: 1.0
Content-Type: text/plain

This is demo 01.
--\x20;
Marco Maggi
")

(define date
  (px.strftime/string "%F %T" (px.localtime (px.time))))

(define message
  (format message-template date))

(define rv
  (sendmail (string->ascii message)))

(display rv)
(newline)
(flush-output-port (current-output-port))

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
