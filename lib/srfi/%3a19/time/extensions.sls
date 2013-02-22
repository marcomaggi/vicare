;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: extensions to SRFI 19, time functions
;;;Date: Wed Jan 23, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright 2010 Derick Eddington.  My MIT-style license is in the file
;;;named LICENSE from  the original collection this  file is distributed
;;;with.
;;;
;;;Modified and extended by Marco Maggi <marco.maggi-ipsu@poste.it>
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
;;;


#!r6rs
(library (srfi :19 time extensions)
  (export
    ;; constants
    NUMBER-OF-NANOSECONDS-IN-A-SECOND
    NUMBER-OF-SECONDS-IN-A-DAY
    NUMBER-OF-SECONDS-IN-HALF-A-DAY
    TAI-EPOCH-IN-JULIAN-DAYS
    NUMBER-OF-TAI-NANOSECONDS-IN-BIZARRE-UTC-SECOND
    UTC-SECONDS-AT-BEG-OF-BIZARRE-SECOND
    UTC-SECONDS-AT-END-OF-BIZARRE-SECOND
    TAI-SECONDS-AT-BEG-OF-BIZARRE-SECOND
    TAI-SECONDS-AT-END-OF-BIZARRE-SECOND

    ;; extension functions
    LEAP-SECONDS-TABLE
    utc-seconds-in-leap-second?		tai-seconds-in-leap-second?
    leap-seconds-table.utc-leap-second-end
    leap-seconds-table.utc-to-tai-increment-after-leap-second

    quasi-time=?)
  (import (srfi :19 time core)))

;;; end of file
