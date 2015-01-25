;;;
;;;Part of: Vicare Scheme
;;;Contents: demo program to send mail with mailx
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
  (vicare posix mailx))

(define (send-it message-template)
  (define date
    (px.strftime/string "%F %T" (px.localtime (px.time))))
  (define message
    (format message-template date))
  (define-values (out err)
    (mailx (string->ascii message)))
  (display out)
  (newline)
  (display err)
  (newline)
  (flush-output-port (current-output-port)))

(send-it "From: <marco@localhost>
To: <marco@localhost>
Subject: demo ~a
MIME-Version: 1.0
Content-Type: text/plain; charset=us-ascii
Content-Transfer-Encoding: 7bit

This is demo 01.
--\x20;
Marco Maggi
")

;;; --------------------------------------------------------------------

;;We are going to use MIME stuff below.  For an introduction to MIME see:
;;
;;   <https://en.wikipedia.org/wiki/MIME>
;;

;;Use "Content-Type: multipart/mixed"  to signal that the message  has multiple parts
;;containing different contents.
;;
(send-it "From: <marco@localhost>
To: <marco@localhost>
Subject: html demo ~a
MIME-Version: 1.0
Content-Type: multipart/mixed; boundary=1234567890/localhost
Content-Transfer-Encoding: 7bit

This is a MIME-encapsulated message

--1234567890/localhost
Content-Type: text/plain

This is the plain part.
--1234567890/localhost
Content-Type: text/html

<html>
<head>
<title>HTML E-mail</title>
</head>
<body>
<p>This is the HTML part. <a href='http://github.com'>Click Here</a></p>
</body>
</html>
--1234567890/localhost--
")

;;Use "Content-Type: multipart/alternative"  to signal that the  message has multiple
;;parts containing alternative versions of the same content.
;;
(send-it "From: <marco@localhost>
To: <marco@localhost>
Subject: html demo ~a
MIME-Version: 1.0
Content-Type: multipart/alternative; boundary=1234567890/localhost
Content-Transfer-Encoding: 7bit

This is a MIME-encapsulated message

--1234567890/localhost
Content-Type: text/plain

This is the plain text.
--1234567890/localhost
Content-Type: text/html

<html>
<head>
<title>HTML E-mail</title>
</head>
<body>
<p>This is the HTML text. <a href='http://github.com'>Click Here</a></p>
</body>
</html>
--1234567890/localhost--
")

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
