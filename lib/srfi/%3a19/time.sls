;;;Copyright 2010 Derick Eddington.  My MIT-style license is in the file
;;;named LICENSE from  the original collection this  file is distributed
;;;with.

;;;Modified and extended by Marco Maggi <marco.maggi-ipsu@poste.it>

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
(library (srfi :19 time)
  (export
    time-duration		time-monotonic		time-process
    time-tai			time-thread		time-utc

    current-date		current-julian-day	current-modified-julian-day
    current-time		time-resolution

    make-time
    copy-time			time?
    time-type			set-time-type!
    time-second			set-time-second!
    time-nanosecond		set-time-nanosecond!

    time=?
    time<=?			time<?
    time>=?			time>?
    time-difference		time-difference!
    add-duration		add-duration!
    subtract-duration		subtract-duration!

    make-date			date?
    date-nanosecond		date-second
    date-minute			date-hour
    date-day			date-month
    date-year			date-zone-offset
    date-year-day		date-week-day
    date-week-number

    date->julian-day
    date->modified-julian-day
    date->time-monotonic
    date->time-tai
    date->time-utc

    julian-day->date
    julian-day->time-monotonic
    julian-day->time-tai
    julian-day->time-utc

    modified-julian-day->date
    modified-julian-day->time-monotonic
    modified-julian-day->time-tai
    modified-julian-day->time-utc

    time-monotonic->date
    time-monotonic->julian-day
    time-monotonic->modified-julian-day
    time-monotonic->time-tai
    time-monotonic->time-tai!
    time-monotonic->time-utc
    time-monotonic->time-utc!

    time-tai->date
    time-tai->julian-day
    time-tai->modified-julian-day
    time-tai->time-monotonic
    time-tai->time-monotonic!
    time-tai->time-utc
    time-tai->time-utc!

    time-utc->date
    time-utc->julian-day
    time-utc->modified-julian-day
    time-utc->time-monotonic
    time-utc->time-monotonic!
    time-utc->time-tai
    time-utc->time-tai!

    date->string			string->date)
  (import (srfi :19 time core)))

;;; end of file
