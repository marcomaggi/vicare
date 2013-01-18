;;;Copyright 2010 Derick Eddington.  My MIT-style license is in the file
;;;named LICENSE from  the original collection this  file is distributed
;;;with.

;;SRFI-19: Time Data Types and Procedures.
;;
;;Copyright (C) I/NET, Inc. (2000, 2002, 2003). All Rights Reserved.
;;
;;This document  and translations of it  may be copied and  furnished to
;;others, and derivative  works that comment on or  otherwise explain it
;;or assist in its implementation may be prepared, copied, published and
;;distributed, in  whole or  in part, without  restriction of  any kind,
;;provided  that  the above  copyright  notice  and this  paragraph  are
;;included  on all  such  copies and  derivative  works.  However,  this
;;document itself  may not be modified  in any way, such  as by removing
;;the  copyright  notice  or  references   to  the  Scheme  Request  For
;;Implementation process or editors, except as needed for the purpose of
;;developing SRFIs in  which case the procedures  for copyrights defined
;;in the SRFI  process must be followed, or as  required to translate it
;;into languages other than English.
;;
;;The limited  permissions granted above  are perpetual and will  not be
;;revoked by the authors or their successors or assigns.
;;
;;This document and  the information contained herein is  provided on an
;;"AS  IS" basis  and  THE  AUTHOR AND  THE  SRFI  EDITORS DISCLAIM  ALL
;;WARRANTIES,  EXPRESS OR  IMPLIED,  INCLUDING BUT  NOT  LIMITED TO  ANY
;;WARRANTY THAT THE USE OF THE  INFORMATION HEREIN WILL NOT INFRINGE ANY
;;RIGHTS OR ANY  IMPLIED WARRANTIES OF MERCHANTABILITY OR  FITNESS FOR A
;;PARTICULAR PURPOSE.


#!r6rs
(import (vicare)
  (prefix (srfi :19) srfi.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI libraries: SRFI 19, time\n")


(parametrise ((check-test-name	'constants))

  (check srfi.time-duration	=> 'time-duration)
  (check srfi.time-monotonic	=> 'time-monotonic)
  (check srfi.time-process	=> 'time-process)
  (check srfi.time-tai		=> 'time-tai)
  (check srfi.time-thread	=> 'time-thread)
  (check srfi.time-utc		=> 'time-utc)

  #t)


(parametrise ((check-test-name	'current))

  (when #f
    (check-pretty-print (srfi.current-date))
    (check-pretty-print (srfi.current-time))
    (check-pretty-print (list 'julian-day (srfi.current-julian-day)
			      'modified-julian-day (srfi.current-modified-julian-day))))

  (check
      (srfi.date? (srfi.current-date))
    => #t)

  (check
      (srfi.time? (srfi.current-time))
    => #t)

  (check
      (number? (srfi.current-julian-day))
    => #t)

  (check
      (number? (srfi.current-modified-julian-day))
    => #t)

  (check
      (srfi.time-resolution)
    => #e1e3)

  #t)


(parametrise ((check-test-name	'time-object))

  (check
      (srfi.time? (srfi.make-time srfi.time-utc 1 2))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (srfi.time? (srfi.copy-time (srfi.make-time srfi.time-utc 1 2)))
    => #t)

  (check
      (let* ((T1 (srfi.make-time srfi.time-utc 1 2))
	     (T2 (srfi.copy-time T1)))
	(list (srfi.time-type T2)
	      (srfi.time-second T2)
	      (srfi.time-nanosecond T2)))
    => (list srfi.time-utc 2 1))

;;; --------------------------------------------------------------------

  (check
      (srfi.time-type (srfi.make-time srfi.time-utc 1 2))
    => srfi.time-utc)

  (check
      (srfi.time-second (srfi.make-time srfi.time-utc 1 2))
    => 2)

  (check
      (srfi.time-nanosecond (srfi.make-time srfi.time-utc 1 2))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (let ((T (srfi.make-time srfi.time-utc 1 2)))
	(srfi.set-time-type! T srfi.time-tai)
	(srfi.time-type T))
    => srfi.time-tai)

  (check
      (let ((T (srfi.make-time srfi.time-utc 1 2)))
	(srfi.set-time-second! T 3)
	(srfi.time-second T))
    => 3)

  (check
      (let ((T (srfi.make-time srfi.time-utc 1 2)))
	(srfi.set-time-nanosecond! T 3)
	(srfi.time-nanosecond T))
    => 3)

  #t)


(parametrise ((check-test-name	'time-compar))

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 2))
	    (T2 (srfi.make-time srfi.time-utc 1 2)))
	(srfi.time=? T1 T2))
    => #t)

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 2))
	    (T2 (srfi.make-time srfi.time-utc 9 0)))
	(srfi.time=? T1 T2))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 2))
	    (T2 (srfi.make-time srfi.time-utc 1 3)))
	(srfi.time<? T1 T2))
    => #t)

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 2))
	    (T2 (srfi.make-time srfi.time-utc 2 2)))
	(srfi.time<? T1 T2))
    => #t)

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 2))
	    (T2 (srfi.make-time srfi.time-utc 1 2)))
	(srfi.time<? T1 T2))
    => #f)

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 3))
	    (T2 (srfi.make-time srfi.time-utc 1 2)))
	(srfi.time<? T1 T2))
    => #f)

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 2 2))
	    (T2 (srfi.make-time srfi.time-utc 1 2)))
	(srfi.time<? T1 T2))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 2))
	    (T2 (srfi.make-time srfi.time-utc 1 3)))
	(srfi.time<=? T1 T2))
    => #t)

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 2))
	    (T2 (srfi.make-time srfi.time-utc 2 2)))
	(srfi.time<=? T1 T2))
    => #t)

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 2))
	    (T2 (srfi.make-time srfi.time-utc 1 2)))
	(srfi.time<=? T1 T2))
    => #t)

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 3))
	    (T2 (srfi.make-time srfi.time-utc 1 2)))
	(srfi.time<=? T1 T2))
    => #f)

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 2 2))
	    (T2 (srfi.make-time srfi.time-utc 1 2)))
	(srfi.time<=? T1 T2))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 2))
	    (T2 (srfi.make-time srfi.time-utc 1 3)))
	(srfi.time>? T1 T2))
    => #f)

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 2))
	    (T2 (srfi.make-time srfi.time-utc 2 2)))
	(srfi.time>? T1 T2))
    => #f)

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 2))
	    (T2 (srfi.make-time srfi.time-utc 1 2)))
	(srfi.time>? T1 T2))
    => #f)

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 3))
	    (T2 (srfi.make-time srfi.time-utc 1 2)))
	(srfi.time>? T1 T2))
    => #t)

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 2 2))
	    (T2 (srfi.make-time srfi.time-utc 1 2)))
	(srfi.time>? T1 T2))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 2))
	    (T2 (srfi.make-time srfi.time-utc 1 3)))
	(srfi.time>=? T1 T2))
    => #f)

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 2))
	    (T2 (srfi.make-time srfi.time-utc 2 2)))
	(srfi.time>=? T1 T2))
    => #f)

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 2))
	    (T2 (srfi.make-time srfi.time-utc 1 2)))
	(srfi.time>=? T1 T2))
    => #t)

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 3))
	    (T2 (srfi.make-time srfi.time-utc 1 2)))
	(srfi.time>=? T1 T2))
    => #t)

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 2 2))
	    (T2 (srfi.make-time srfi.time-utc 1 2)))
	(srfi.time>=? T1 T2))
    => #t)

  #t)


(parametrise ((check-test-name	'time-arith))

;;; difference

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 2))
	    (T2 (srfi.make-time srfi.time-utc 1 2)))
	(srfi.time-difference T1 T2))
    (=> srfi.time=?)
    (srfi.make-time srfi.time-duration 0 0))

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 2 3))
	    (T2 (srfi.make-time srfi.time-utc 1 2)))
	(srfi.time-difference T1 T2))
    (=> srfi.time=?)
    (srfi.make-time srfi.time-duration 1 1))

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 2))
	    (T2 (srfi.make-time srfi.time-utc 2 3)))
	(srfi.time-difference T1 T2))
    (=> srfi.time=?)
    (srfi.make-time srfi.time-duration -1 -1))

;;; --------------------------------------------------------------------
;;; difference!

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 2))
	    (T2 (srfi.make-time srfi.time-utc 1 2)))
	(srfi.time-difference! T1 T2)
	T1)
    (=> srfi.time=?)
    (srfi.make-time srfi.time-duration 0 0))

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 2 3))
	    (T2 (srfi.make-time srfi.time-utc 1 2)))
	(srfi.time-difference! T1 T2)
	T1)
    (=> srfi.time=?)
    (srfi.make-time srfi.time-duration 1 1))

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 2))
	    (T2 (srfi.make-time srfi.time-utc 2 3)))
	(srfi.time-difference! T1 T2)
	T1)
    (=> srfi.time=?)
    (srfi.make-time srfi.time-duration -1 -1))

;;; --------------------------------------------------------------------
;;; add

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 2))
	    (T2 (srfi.make-time srfi.time-duration 10 20)))
	(srfi.add-duration T1 T2))
    (=> srfi.time=?)
    (srfi.make-time srfi.time-utc 11 22))

;;; --------------------------------------------------------------------
;;; add!

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 1 2))
	    (T2 (srfi.make-time srfi.time-duration 10 20)))
	(srfi.add-duration! T1 T2)
	T1)
    (=> srfi.time=?)
    (srfi.make-time srfi.time-utc 11 22))

;;; --------------------------------------------------------------------
;;; subtract

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 11 22))
	    (T2 (srfi.make-time srfi.time-duration 1 2)))
	(srfi.subtract-duration T1 T2))
    (=> srfi.time=?)
    (srfi.make-time srfi.time-utc 10 20))

;;; --------------------------------------------------------------------
;;; subtract!

  (check
      (let ((T1 (srfi.make-time srfi.time-utc 11 22))
	    (T2 (srfi.make-time srfi.time-duration 1 2)))
	(srfi.subtract-duration! T1 T2)
	T1)
    (=> srfi.time=?)
    (srfi.make-time srfi.time-utc 10 20))

  #t)


(parametrise ((check-test-name	'date-object))

  (check
      (let ((D (srfi.make-date 123	;nanosecond
			       45	;second
			       10	;minute
			       6	;hour
			       22	;day
			       2	;month
			       2013	;year
			       3600	;Rome
			       )))
	(srfi.date? D))
    => #t)

  (let-syntax
      ((check-leap-second-date
	(syntax-rules ()
	  ((_ ?year ?month ?day)
	   (check
	       (let ((D (srfi.make-date 0 60 59 23 ?day ?month ?year 0)))
		 (srfi.date-second D))
	     => 60)))))
    ;;All of these are the leap seconds dates.
    (check-leap-second-date 1972	6	30)
    (check-leap-second-date 1972	12	31)
    (check-leap-second-date 1973	12	31)
    (check-leap-second-date 1974	12	31)
    (check-leap-second-date 1975	12	31)
    (check-leap-second-date 1976	12	31)
    (check-leap-second-date 1977	12	31)
    (check-leap-second-date 1978	12	31)
    (check-leap-second-date 1979	12	31)
    (check-leap-second-date 1981	6	30)
    (check-leap-second-date 1982	6	30)
    (check-leap-second-date 1983	6	30)
    (check-leap-second-date 1985	6	30)
    (check-leap-second-date 1987	12	31)
    (check-leap-second-date 1989	12	31)
    (check-leap-second-date 1990	12	31)
    (check-leap-second-date 1992	6	30)
    (check-leap-second-date 1993	6	30)
    (check-leap-second-date 1994	6	30)
    (check-leap-second-date 1995	12	31)
    (check-leap-second-date 1997	6	30)
    (check-leap-second-date 1998	12	31)
    (check-leap-second-date 2005	12	31)
    (check-leap-second-date 2008	12	31)
    (check-leap-second-date 2012	6	30)
    #f)

  (check
      (let ((D (srfi.make-date 0 59 59 23 18 1 2013 0)))
	(srfi.date-second D))
    => 59)

  (check
      (guard (E ((assertion-violation? E)
		 (condition-message E))
		(else E))
	(let ((D (srfi.make-date 0 60 59 23 18 1 2013 0)))
	  (srfi.date-second D)))
    => "expected exact integer in range [0, 59] or [0, 60] as number of seconds argument")

  #t)


;;;; done

(check-report)

;;; end of file
