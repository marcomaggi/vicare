;;;read-table.sps --
;;;
;;;	Script that reads a file selected on the command line:
;;;
;;;	   $ vicare read-table.sps -- tai-utc.dat
;;;
;;;	and converts the data into a  Scheme alist.  The alist is prited
;;;	to stdout.  The data file should be available from:
;;;
;;;		<ftp://maia.usno.navy.mil/ser7/tai-utc.dat>
;;;
;;;	but it  appears that sometimes it  is not.  If this  happens try
;;;	this URL:
;;;
;;;	   <http://www.physics.wisc.edu/~craigm/idl/down/tai_utc.pro>
;;;
;;;Copyright 2010 Derick Eddington.  My MIT-style license is in the file
;;;named LICENSE from  the original collection this  file is distributed
;;;with.
;;;
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;SRFI-19: Time Data Types and Procedures.
;;;
;;;Copyright (C) I/NET, Inc. (2000, 2002, 2003). All Rights Reserved.
;;;
;;;This document and  translations of it may be copied  and furnished to
;;;others, and derivative works that  comment on or otherwise explain it
;;;or assist  in its implementation  may be prepared,  copied, published
;;;and  distributed, in  whole or  in part,  without restriction  of any
;;;kind, provided that the above copyright notice and this paragraph are
;;;included  on all  such copies  and derivative  works.  However,  this
;;;document itself may  not be modified in any way,  such as by removing
;;;the  copyright  notice  or  references  to  the  Scheme  Request  For
;;;Implementation process or  editors, except as needed  for the purpose
;;;of  developing SRFIs  in  which case  the  procedures for  copyrights
;;;defined  in the  SRFI process  must be  followed, or  as required  to
;;;translate it into languages other than English.
;;;
;;;The limited permissions  granted above are perpetual and  will not be
;;;revoked by the authors or their successors or assigns.
;;;
;;;This document and the information  contained herein is provided on an
;;;"AS  IS" basis  and  THE AUTHOR  AND THE  SRFI  EDITORS DISCLAIM  ALL
;;;WARRANTIES,  EXPRESS OR  IMPLIED, INCLUDING  BUT NOT  LIMITED TO  ANY
;;;WARRANTY THAT THE USE OF THE INFORMATION HEREIN WILL NOT INFRINGE ANY
;;;RIGHTS OR ANY IMPLIED WARRANTIES  OF MERCHANTABILITY OR FITNESS FOR A
;;;PARTICULAR PURPOSE.


#!r6rs
(import (vicare))

(module (read-tai-utc-data)

  (define (read-tai-utc-data filename)
    (let ((port  (open-input-file filename))
	  (table '()))
      (let loop ((line (get-line port)))
	(unless (eof-object? line)
	  ;;Convert a  line of text  from the data  file into a  list of
	  ;;Scheme values.
	  (let ((port (open-string-input-port (string-append "(" line ")"))))
	    (let* ((data (read   port))
		   ;;The year in which the leap second is introduced.
		   (year (car    data))
		   ;;The  Julian  Date  in  which  the  leap  second  is
		   ;;introduced.
		   (jd   (cadddr (cdr data)))
		   ;;The  number of  seconds,  at such  Julian Date,  by
		   ;;which UTC is behind TAI.
		   (secs (cadddr (cdddr data))))
	      ;;The first leap second was  added in 1972; we assume that
	      ;;TAI and UTC were synchronised before such leap second.
	      (when (>= year 1972)
		(set! table (cons (cons (convert-jd  jd)
					(convert-sec secs))
				  table)))))
	  (loop (get-line port))))
      table))

  (define (convert-jd jd)
    ;;Convert the Julian  Date from the representation in  the data file
    ;;to an exact  integer representing the number of  seconds since the
    ;;Epoch in the UTC scale.
    ;;
    ;;The Julian  Date represents a  point in time  as a real  number of
    ;;days  since -4714-11-24T12:00:00Z  (November  24,  -4714 at  noon,
    ;;UTC).
    ;;
    (* (- (exact jd) TAI-EPOCH-IN-JULIAN-DAYS) NUMBER-OF-SECONDS-IN-A-DAY))

  (define convert-sec
    ;;Convert the number of seconds  from the representation in the data
    ;;file to an exact integer.
    exact)

  (define NUMBER-OF-SECONDS-IN-A-DAY	86400)
  (define TAI-EPOCH-IN-JULIAN-DAYS	4881175/2)

  #| end of module |# )

(define LEAP-SECOND-TABLE
  (read-tai-utc-data (cadr (command-line))))

(display "(")
(let loop ((ell LEAP-SECOND-TABLE))
  (if (null? ell)
      (begin
	(display ")\n")
	(flush-output-port (current-output-port)))
    (begin
      (display (car ell))
      (unless (null? (cdr ell))
	(display "\n"))
      (loop (cdr ell)))))

;;; end of file
