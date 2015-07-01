;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: the script from SRFI 19 to be used to regenerate the table
;;;Date: Fri Sep 17, 2010
;;;
;;;Abstract
;;;
;;;	Read the table of leap seconds from:
;;;
;;;		<ftp://maia.usno.navy.mil/ser7/tai-utc.dat>
;;;
;;;	and print a Scheme alist to be used in the (times-and-dates seconds) library.
;;;	This script comes from  SRFI 19.  It may be required to  edit this data file,
;;;	for example to break the occurrences of "0.0011232S" into "0.0011232 S"; this
;;;	is because the script does not implement a proper parser.
;;;
;;;	We should run this script as:
;;;
;;;	   $ vicare --r6rs-script read-leap-seconds-table.sps -- tai-utc.dat
;;;
;;;	the alist is displayed on the standard output port.
;;;
;;;Copyright (C) I/NET, Inc. (2000, 2002, 2003).  All Rights Reserved.
;;;Modified by Marco Maggi.
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
(import (rnrs))

(define NUMBER-OF-SECONDS-IN-ONE-DAY (* 60 60 24))
(define TAI-EPOCH-IN-JD 4881175/2)

(define (read-tai-utc-data filename)
  (define (convert-jd jd)
    (* (- (exact jd) TAI-EPOCH-IN-JD) NUMBER-OF-SECONDS-IN-ONE-DAY))
  (define (convert-sec sec)
    (exact sec))
  (let ((port  (open-input-file filename))
	(table '()))
    (let loop ((line (get-line port)))
      (unless (eof-object? line)
	(let ((port (open-string-input-port (string-append "(" line ")"))))
	  (let* ((data (read   port))
		 (year (car    data))
		 (jd   (cadddr (cdr data)))
		 (secs (cadddr (cdddr data))))
	    (when (>= year 1972)
	      (set! table (cons (cons (convert-jd jd) (convert-sec secs)) table)))))
	(loop (get-line port))))
    table))

(define (read-leap-second-table filename)
  (set! $leap-second-table (read-tai-utc-data filename))
  (values))

(define $leap-second-table
  (read-tai-utc-data (cadr (command-line))))
(display "(")
(map (lambda (entry)
       (write entry)
       (newline))
  $leap-second-table)
(display ")")
(newline)
(flush-output-port (current-output-port))

;;; end of file
