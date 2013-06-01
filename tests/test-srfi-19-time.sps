;;;Copyright 2010 Derick Eddington.  My MIT-style license is in the file
;;;named LICENSE from  the original collection this  file is distributed
;;;with.

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

;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>


#!r6rs
(import (vicare)
  (prefix (srfi :19)
	  srfi.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI libraries: SRFI 19, time\n")


(parametrise ((check-test-name	'time-structs))

  (check
      (srfi.time? (srfi.current-time 'time-tai))
    => #t)

  (check
      (srfi.time? (srfi.current-time 'time-utc))
    => #t)

  (check
      (srfi.time? (srfi.current-time 'time-monotonic))
    => #t)

  #t)


(parametrise ((check-test-name	'time-resolution))

  (check (srfi.time-resolution 'time-tai)	=> 1000)
  (check (srfi.time-resolution 'time-utc)	=> 1000)
  (check (srfi.time-resolution 'time-monotonic)	=> 1000)

  #t)


(parametrise ((check-test-name	'time-comparison))

  (define t1 (srfi.make-time 'time-utc 0 1))
  (define t2 (srfi.make-time 'time-utc 0 1))
  (define t3 (srfi.make-time 'time-utc 0 2))
  (define t11 (srfi.make-time 'time-utc 1001 1))
  (define t12 (srfi.make-time 'time-utc 1001 1))
  (define t13 (srfi.make-time 'time-utc 1001 2))

  (check (srfi.time=? t1 t2)	=> #t)
  (check (srfi.time>? t3 t2)	=> #t)
  (check (srfi.time<? t2 t3)	=> #t)
  (check (srfi.time>=? t1 t2)	=> #t)
  (check (srfi.time>=? t3 t2)	=> #t)
  (check (srfi.time<=? t1 t2)	=> #t)
  (check (srfi.time<=? t2 t3)	=> #t)
  (check (srfi.time=? t11 t12)	=> #t)
  (check (srfi.time>? t13 t12)	=> #t)
  (check (srfi.time<? t12 t13)	=> #t)
  (check (srfi.time>=? t11 t12)	=> #t)
  (check (srfi.time>=? t13 t12)	=> #t)
  (check (srfi.time<=? t11 t12)	=> #t)
  (check (srfi.time<=? t12 t13)	=> #t)

  #t)


(parametrise ((check-test-name	'time-difference))

  (let ((t1 (srfi.make-time 'time-utc 0 3000))
	(t2 (srfi.make-time 'time-utc 0 1000))
	(t3 (srfi.make-time 'time-duration 0 2000))
	(t4 (srfi.make-time 'time-duration 0 -2000)))
    (check
	(srfi.time=? t3 (srfi.time-difference t1 t2))
      => #t)
    (check
	(srfi.time=? t4 (srfi.time-difference t2 t1))
      => #t))

  (let ((t1 (srfi.make-time srfi.time-utc 1000 3000))
	(t2 (srfi.make-time srfi.time-utc 0 3000))
	(t3 (srfi.make-time srfi.time-duration 1000 0))
	(t4 (srfi.make-time srfi.time-duration 999999000 -1)))
    (check
	(srfi.time=? t3 (srfi.time-difference t1 t2))
      => #t)
    (check
	(srfi.time=? t4 (srfi.time-difference t2 t1))
      => #t))

  #t)


(parametrise ((check-test-name	'utc-tai-edge))

  (define (test-one-utc-tai-edge utc tai-diff tai-last-diff)
    (let* ( ;; right on the edge they should be the same
	   (utc-basic (srfi.make-time 'time-utc 0 utc))
	   (tai-basic (srfi.make-time 'time-tai 0 (+ utc tai-diff)))
	   (utc->tai-basic (srfi.time-utc->time-tai utc-basic))
	   (tai->utc-basic (srfi.time-tai->time-utc tai-basic))
	   ;; a second before they should be the old diff
	   (utc-basic-1 (srfi.make-time 'time-utc 0 (- utc 1)))
	   (tai-basic-1 (srfi.make-time 'time-tai 0 (- (+ utc tai-last-diff) 1)))
	   (utc->tai-basic-1 (srfi.time-utc->time-tai utc-basic-1))
	   (tai->utc-basic-1 (srfi.time-tai->time-utc tai-basic-1))
	   ;; a second later they should be the new diff
	   (utc-basic+1 (srfi.make-time 'time-utc 0 (+ utc 1)))
	   (tai-basic+1 (srfi.make-time 'time-tai 0 (+ (+ utc tai-diff) 1)))
	   (utc->tai-basic+1 (srfi.time-utc->time-tai utc-basic+1))
	   (tai->utc-basic+1 (srfi.time-tai->time-utc tai-basic+1))
	   ;; ok, let's move the clock half a month or so plus half a second
	   (shy (* 15 24 60 60))
	   (hs (/ (expt 10 9) 2))
	   ;; a second later they should be the new diff
	   (utc-basic+2 (srfi.make-time 'time-utc hs (+ utc shy)))
	   (tai-basic+2 (srfi.make-time 'time-tai hs (+ (+ utc tai-diff) shy)))
	   (utc->tai-basic+2 (srfi.time-utc->time-tai utc-basic+2))
	   (tai->utc-basic+2 (srfi.time-tai->time-utc tai-basic+2)))
      (and (srfi.time=? utc-basic tai->utc-basic)
	   (srfi.time=? tai-basic utc->tai-basic)
	   (srfi.time=? utc-basic-1 tai->utc-basic-1)
	   (srfi.time=? tai-basic-1 utc->tai-basic-1)
	   (srfi.time=? utc-basic+1 tai->utc-basic+1)
	   (srfi.time=? tai-basic+1 utc->tai-basic+1)
	   (srfi.time=? utc-basic+2 tai->utc-basic+2)
	   (srfi.time=? tai-basic+2 utc->tai-basic+2))))

  (check (test-one-utc-tai-edge 915148800  32 31)	=> #t)
  (check (test-one-utc-tai-edge 867715200  31 30)	=> #t)
  (check (test-one-utc-tai-edge 820454400  30 29)	=> #t)
  (check (test-one-utc-tai-edge 773020800  29 28)	=> #t)
  (check (test-one-utc-tai-edge 741484800  28 27)	=> #t)
  (check (test-one-utc-tai-edge 709948800  27 26)	=> #t)
  (check (test-one-utc-tai-edge 662688000  26 25)	=> #t)
  (check (test-one-utc-tai-edge 631152000  25 24)	=> #t)
  (check (test-one-utc-tai-edge 567993600  24 23)	=> #t)
  (check (test-one-utc-tai-edge 489024000  23 22)	=> #t)
  (check (test-one-utc-tai-edge 425865600  22 21)	=> #t)
  (check (test-one-utc-tai-edge 394329600  21 20)	=> #t)
  (check (test-one-utc-tai-edge 362793600  20 19)	=> #t)
  (check (test-one-utc-tai-edge 315532800  19 18)	=> #t)
  (check (test-one-utc-tai-edge 283996800  18 17)	=> #t)
  (check (test-one-utc-tai-edge 252460800  17 16)	=> #t)
  (check (test-one-utc-tai-edge 220924800  16 15)	=> #t)
  (check (test-one-utc-tai-edge 189302400  15 14)	=> #t)
  (check (test-one-utc-tai-edge 157766400  14 13)	=> #t)
  (check (test-one-utc-tai-edge 126230400  13 12)	=> #t)
  (check (test-one-utc-tai-edge 94694400   12 11)	=> #t)
  (check (test-one-utc-tai-edge 78796800   11 10)	=> #t)
  (check (test-one-utc-tai-edge 63072000   10 0)	=> #t)
  ;; at the epoch
  (check (test-one-utc-tai-edge 0   0 0)	      	=> #t)
  ;; close to it ...
  (check (test-one-utc-tai-edge 10   0 0)	      	=> #t)
  ;; about now ...
  (check (test-one-utc-tai-edge 1045789645 32 32)    	=> #t)

  #t)


(parametrise ((check-test-name	'date-comparison))

  (define (tm:date= d1 d2)
    (and (= (srfi.date-year d1) (srfi.date-year d2))
	 (= (srfi.date-month d1) (srfi.date-month d2))
	 (= (srfi.date-day d1) (srfi.date-day d2))
	 (= (srfi.date-hour d1) (srfi.date-hour d2))
	 (= (srfi.date-second d1) (srfi.date-second d2))
	 (= (srfi.date-nanosecond d1) (srfi.date-nanosecond d2))
	 (= (srfi.date-zone-offset d1) (srfi.date-zone-offset d2))))

  (check
      (tm:date= (srfi.time-tai->date (srfi.make-time srfi.time-tai 0 (+ 915148800 29)) 0)
		(srfi.make-date 0 58 59 23 31 12 1998 0))
    => #t)

  (check
      (tm:date= (srfi.time-tai->date (srfi.make-time srfi.time-tai 0 (+ 915148800 30)) 0)
		(srfi.make-date 0 59 59 23 31 12 1998 0))
    => #t)

  (check
      (tm:date= (srfi.time-tai->date (srfi.make-time srfi.time-tai 0 (+ 915148800 31)) 0)
		(srfi.make-date 0 60 59 23 31 12 1998 0))
    => #t)

  (check
      (tm:date= (srfi.time-tai->date (srfi.make-time srfi.time-tai 0 (+ 915148800 32)) 0)
		(srfi.make-date 0 0 0 0 1 1 1999 0))
    => #t)

  #t)


(parametrise ((check-test-name	'date-utc-conversion))

  (check
      (srfi.time=? (srfi.make-time srfi.time-utc 0 (- 915148800 2))
		   (srfi.date->time-utc (srfi.make-date 0 58 59 23 31 12 1998 0)))
    => #t)

  (check
      (srfi.time=? (srfi.make-time srfi.time-utc 0 (- 915148800 1))
		   (srfi.date->time-utc (srfi.make-date 0 59 59 23 31 12 1998 0)))
    => #t)

  ;; yes, I think this is acutally right.
  (check
      (srfi.time=? (srfi.make-time srfi.time-utc 0 (- 915148800 0))
		   (srfi.date->time-utc (srfi.make-date 0 60 59 23 31 12 1998 0)))
    => #t)

  (check
      (srfi.time=? (srfi.make-time srfi.time-utc 0 (- 915148800 0))
		   (srfi.date->time-utc (srfi.make-date 0 0 0 0 1 1 1999 0)))
    => #t)

  (check
      (srfi.time=? (srfi.make-time srfi.time-utc 0 (+ 915148800 1))
		   (srfi.date->time-utc (srfi.make-date 0 1 0 0 1 1 1999 0)))
    => #t)

  #t)


(parametrise ((check-test-name	'tz-offset-conversion))

  (let ((ct-utc (srfi.make-time srfi.time-utc 6320000 1045944859))
	(ct-tai (srfi.make-time srfi.time-tai 6320000 1045944891))
	(cd (srfi.make-date 6320000 19 14 15 22 2 2003 -18000)))
    (check
	(srfi.time=? ct-utc (srfi.date->time-utc cd))
      => #t)
    (check
	(srfi.time=? ct-tai (srfi.date->time-tai cd))
      => #t))

  #t)


(parametrise ((check-test-name	'date-string))

  (define (printf fmt-str . args)
    (display (apply format fmt-str args)))

  (define (date->string/all-formats)
    ;; NOTE: ~x and ~X aren't doing what the SRFI 19 document says they do.
    ;;       I guess that's a bug in the reference implementation.
    (define fs
      '("~~" "~a" "~A" "~b" "~B" "~c" "~d" "~D" "~e" "~f" "~h" "~H" "~I" "~j" "~k"
	"~l" "~m" "~M" "~n" "~N" "~p" "~r" "~s" "~S" "~t" "~T" "~U" "~V" "~w" "~W"
	"~x" "~X" "~y" "~Y" "~z" "~Z" "~1" "~2" "~3" "~4" "~5"))
    (define cd (srfi.current-date))
    (display "\n;;; Running date->string format exercise\n")
    (printf "(current-date) => ~s\n" cd)
    (for-each
	(lambda (f)
	  (printf "Format: ~a: " f)
	  (display (srfi.date->string cd f)) (newline))
      fs))

  (date->string/all-formats)
  (flush-output-port (current-output-port))

  #t)


;;;; done

(check-report)

;;; end of file
