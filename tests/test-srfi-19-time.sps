;;;Copyright 2010, 2013 Derick Eddington.  My MIT-style license is in the file
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
  (prefix (srfi :19 time extensions) srfi.)
  (vicare checks))

(check-set-mode! 'report-failed)
#;(check-set-mode! 'summary)
(check-display "*** testing SRFI libraries: SRFI 19, time\n")


;;;; helpers

(define UTC-SECONDS-AT-BEG-OF-BIZARRE-SECOND (- 63072000 1))
(define UTC-SECONDS-AT-END-OF-BIZARRE-SECOND 63072000)

(define TAI-SECONDS-AT-BEG-OF-BIZARRE-SECOND (- 63072000 1))
(define TAI-SECONDS-AT-END-OF-BIZARRE-SECOND (+ 63072000 10))

(define NUMBER-OF-NANOSECONDS-IN-A-SECOND	#e1e9)


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


(parametrise ((check-test-name	'leap-second))

  (let loop ((table srfi.LEAP-SECONDS-TABLE))
    (unless (null? table)
      (let ((SEC (leap-seconds-table.utc-leap-second-end (car table))))
	(check (srfi.utc-seconds-in-leap-second? SEC)		=> #f)
	(check (srfi.utc-seconds-in-leap-second? (+ -1 SEC))	=> #t)
	(check (srfi.utc-seconds-in-leap-second? (+ -2 SEC))	=> #f))
      (loop (cdr table))))

  (let ((SEC 63072000))
    (check (srfi.utc-seconds-in-leap-second? SEC)		=> #f)
    (check (srfi.utc-seconds-in-leap-second? (+ -1 SEC))	=> #f)
    (check (srfi.utc-seconds-in-leap-second? (+ -2 SEC))	=> #f))

;;; --------------------------------------------------------------------

  (let loop ((table UTC-SECONDS-AFTER-LEAP-SECONDS))
    (unless (null? table)
      (let* ((entry (car table))
	     (SEC   (srfi.leap-seconds-table.utc-leap-second-end entry))
	     (DELTA (srfi.leap-seconds-table.utc-to-tai-increment-after-leap-second entry)))
	(check (srfi.tai-seconds-in-leap-second? SEC)		=> #f)
	(check (srfi.tai-seconds-in-leap-second? (+ -1 SEC))	=> #t)
	(check (srfi.tai-seconds-in-leap-second? (+ -2 DELTA SEC))	=> #f))
      (loop (cdr table))))

  (let ((SEC 63072000))
    (check (srfi.tai-seconds-in-leap-second? SEC)		=> #f)
    (check (srfi.tai-seconds-in-leap-second? (+ -1 SEC))	=> #f)
    (check (srfi.tai-seconds-in-leap-second? (+ -2 SEC))	=> #f))

  #t)


(parametrise ((check-test-name	'utc-to-tai))

  (define utc->tai
    (case-lambda
     ((utc-seconds tai-seconds nanoseconds)
      (utc->tai utc-seconds tai-seconds nanoseconds #f))
     ((utc-seconds tai-seconds nanoseconds print?)
      (check
	  (let* ((UTC (srfi.make-time srfi.time-utc nanoseconds utc-seconds))
		 (TAI (srfi.time-utc->time-tai UTC)))
	    (when print?
	      (check-pretty-print UTC)
	      (check-pretty-print TAI))
	    TAI)
	(=> srfi.time=?)
	(srfi.make-time srfi.time-tai nanoseconds tai-seconds)))))

  (define utc->tai*
    (case-lambda
     ((utc-seconds tai-seconds)
      (utc->tai* utc-seconds tai-seconds #f))
     ((utc-seconds tai-seconds print?)
      (utc->tai utc-seconds tai-seconds 0 print?)
      (utc->tai utc-seconds tai-seconds #e5e8 print?)
      (utc->tai utc-seconds tai-seconds (- NUMBER-OF-NANOSECONDS-IN-A-SECOND 1) print?))))

;;; --------------------------------------------------------------------

  ;;The Epoch: 0[TAI] = 0[UTC]
  #;(let* ((TAI-SECONDS	0)
	 (UTC-SECONDS	TAI-SECONDS))
    (utc->tai* UTC-SECONDS TAI-SECONDS))

  ;;One second before the bizarre UTC second: 63072000-12[TAI] = 63072000-2[UTC]
  #;(let* ((UTC-SECONDS	(- UTC-SECONDS-AT-END-OF-BIZARRE-SECOND 12))
	 (TAI-SECONDS	UTC-SECONDS))
    (utc->tai* UTC-SECONDS TAI-SECONDS))

  ;;In the bizarre UTC second:
  ;;
  ;;   (63072000-1 0)[TAI]	=>	(63072000-1         0)[UTC]
  ;;
  ;;   (63072000+1 0)[TAI]	=>	(63072000-1 100000000)[UTC]
  ;;   (63072000+2 0)[TAI]	=>	(63072000-1 200000000)[UTC]
  ;;   (63072010+3 0)[TAI]	=>	(63072000-1 300000000)[UTC]
  ;;   (63072010+4 0)[TAI]	=>	(63072000-1 400000000)[UTC]
  ;;   (63072010+5 0)[TAI]	=>	(63072000-1 500000000)[UTC]
  ;;   (63072010+6 0)[TAI]	=>	(63072000-1 600000000)[UTC]
  ;;   (63072010+7 0)[TAI]	=>	(63072000-1 700000000)[UTC]
  ;;   (63072010+8 0)[TAI]	=>	(63072000-1 800000000)[UTC]
  ;;   (63072010-01 0)[TAI]	=>	(63072000-1 900000000)[UTC]
  ;;   (63072010    0)[TAI]	=>	(63072000           0)[UTC]
  ;;
  (let ()
    (define (utc->tai utc-seconds utc-nanoseconds tai-seconds tai-nanoseconds print?)
      (check
	  (let* ((UTC (srfi.make-time srfi.time-utc utc-nanoseconds utc-seconds))
		 (TAI (srfi.time-utc->time-tai UTC)))
	    (when print?
	      (check-pretty-print UTC)
	      (check-pretty-print TAI))
	    TAI)
	(=> (lambda (a b)
	      (srfi.quasi-time=? a b 10)))
	(srfi.make-time srfi.time-tai tai-nanoseconds tai-seconds)))
    (utc->tai UTC-SECONDS-AT-END-OF-BIZARRE-SECOND 0
	      TAI-SECONDS-AT-END-OF-BIZARRE-SECOND 0
	      #f)
    (do ((i 1 (+ 1 i)))
	((= i 11))
      (utc->tai UTC-SECONDS-AT-BEG-OF-BIZARRE-SECOND
		(* NUMBER-OF-NANOSECONDS-IN-A-SECOND (/ i 11))
		(+ TAI-SECONDS-AT-BEG-OF-BIZARRE-SECOND i) 0
		#f))
    (utc->tai UTC-SECONDS-AT-BEG-OF-BIZARRE-SECOND 0
	      TAI-SECONDS-AT-BEG-OF-BIZARRE-SECOND 0
	      #f)
    )

  ;;At the end of the initial delta: 63072010[TAI] = 63072000[UTC]
  #;(let* ((UTC-SECONDS	UTC-SECONDS-AT-END-OF-BIZARRE-SECOND)
	 (TAI-SECONDS	(+ 10 UTC-SECONDS)))
    (check
	(srfi.time-utc->time-tai (srfi.make-time srfi.time-utc 0 UTC-SECONDS))
      (=> srfi.time=?)
      (srfi.make-time srfi.time-tai 0 TAI-SECONDS)))

  ;;Right after the initial delta: 63072000+11[TAI] = 63072000+1[UTC]
  #;(let* ((UTC-SECONDS	(+  1 UTC-SECONDS-AT-END-OF-BIZARRE-SECOND))
	 (TAI-SECONDS	(+ 11 UTC-SECONDS-AT-END-OF-BIZARRE-SECOND)))
    (check
	(srfi.time-utc->time-tai (srfi.make-time srfi.time-utc 0 UTC-SECONDS))
      (=> srfi.time=?)
      (srfi.make-time srfi.time-tai 0 TAI-SECONDS)))

  ;;The full table of leap seconds.
  #;(let loop ((table '((1341100800 . 35)	(1230768000 . 34)	(1136073600 . 33)
		      (915148800 . 32)	(867715200 . 31)	(820454400 . 30)
		      (773020800 . 29)	(741484800 . 28)	(709948800 . 27)
		      (662688000 . 26)	(631152000 . 25)	(567993600 . 24)
		      (489024000 . 23)	(425865600 . 22)	(394329600 . 21)
		      (362793600 . 20)	(315532800 . 19)	(283996800 . 18)
		      (252460800 . 17)	(220924800 . 16)	(189302400 . 15)
		      (157766400 . 14)	(126230400 . 13)	(94694400 . 12)
		      (78796800 . 11)	(63072000 . 10))))
    (unless (null? table)
      (check
	  (let* ((UTC-SECONDS	(caar table))
		 (TAI-SECONDS	(+ UTC-SECONDS (cdar table)))
		 (UTC		(srfi.make-time srfi.time-utc 0 UTC-SECONDS)))
;;;	      (check-pretty-print UTC)
	    (srfi.time-utc->time-tai UTC))
	(=> srfi.time=?)
	(srfi.make-time srfi.time-tai 0 TAI-SECONDS))
      (loop (cdr table))))

;;; --------------------------------------------------------------------

  ;;A random time.
  #;(let* ((UTC-SECONDS	(+ 1000 1341100800))
	 (UTC-NANOS	500000)
	 (TAI-SECONDS	(+ 35 UTC-SECONDS))
	 (TAI-NANOS	UTC-NANOS))
    (check
	(srfi.time-utc->time-tai (srfi.make-time srfi.time-utc UTC-NANOS UTC-SECONDS))
      (=> srfi.time=?)
      (srfi.make-time srfi.time-tai TAI-NANOS TAI-SECONDS)))

  #t)


(parametrise ((check-test-name	'tai-to-utc))

  (define UTC-SECONDS-AFTER-BIZARRE-SECOND 63072000)

;;; --------------------------------------------------------------------

  ;;The Epoch: 0[TAI] = 0[UTC]
  (let* ((TAI-SECONDS	0)
	 (UTC-SECONDS	TAI-SECONDS))
    (check
	(srfi.time-tai->time-utc (srfi.make-time srfi.time-tai 0 TAI-SECONDS))
      (=> srfi.time=?)
      (srfi.make-time srfi.time-utc 0 UTC-SECONDS)))

  ;;Right before the initial delta: 63072000-1[TAI] = 63072000-1[UTC]
  (let* ((UTC-SECONDS	(- UTC-SECONDS-AFTER-BIZARRE-SECOND 1))
	 (TAI-SECONDS	UTC-SECONDS))
    (check
	(srfi.time-tai->time-utc (srfi.make-time srfi.time-tai 0 TAI-SECONDS))
      (=> srfi.time=?)
      (srfi.make-time srfi.time-utc 0 UTC-SECONDS)))

  ;;On the initial delta: 63072010[TAI] = 63072000[UTC]
  (let* ((UTC-SECONDS	UTC-SECONDS-AFTER-BIZARRE-SECOND)
	 (TAI-SECONDS	(+ 10 UTC-SECONDS)))
    (check
	(srfi.time-tai->time-utc (srfi.make-time srfi.time-tai 0 TAI-SECONDS))
      (=> srfi.time=?)
      (srfi.make-time srfi.time-utc 0 UTC-SECONDS)))

  ;;Right after the initial delta: 63072000+11[TAI] = 63072000+1[UTC]
  (let* ((UTC-SECONDS	(+  1 UTC-SECONDS-AFTER-BIZARRE-SECOND))
	 (TAI-SECONDS	(+ 11 UTC-SECONDS-AFTER-BIZARRE-SECOND)))
    (check
	(srfi.time-tai->time-utc (srfi.make-time srfi.time-tai 0 TAI-SECONDS))
      (=> srfi.time=?)
      (srfi.make-time srfi.time-utc 0 UTC-SECONDS)))

  ;;The full table.
  (let loop ((table '((1341100800 . 35)	(1230768000 . 34)	(1136073600 . 33)
		      (915148800 . 32)	(867715200 . 31)	(820454400 . 30)
		      (773020800 . 29)	(741484800 . 28)	(709948800 . 27)
		      (662688000 . 26)	(631152000 . 25)	(567993600 . 24)
		      (489024000 . 23)	(425865600 . 22)	(394329600 . 21)
		      (362793600 . 20)	(315532800 . 19)	(283996800 . 18)
		      (252460800 . 17)	(220924800 . 16)	(189302400 . 15)
		      (157766400 . 14)	(126230400 . 13)	(94694400 . 12)
		      (78796800 . 11)	(63072000 . 10))))
    (unless (null? table)
      (let* ((UTC-SECONDS	(caar table))
	     (TAI-SECONDS	(+ UTC-SECONDS (cdar table))))
	(check
	    (let ((TAI (srfi.make-time srfi.time-tai 0 TAI-SECONDS)))
;;;	      (check-pretty-print TAI)
	      (srfi.time-tai->time-utc TAI))
	  (=> srfi.time=?)
	  (srfi.make-time srfi.time-utc 0 UTC-SECONDS)))
      (loop (cdr table))))

;;; --------------------------------------------------------------------

  ;;Seconds inside the UTC bizarre second.
  (do ((i 0 (+ 1 i)))
      ((= i 11))
    (let* ((TAI-SECONDS	(+ i 63072000))
	   (TAI-NANOS	0)
	   (UTC-SECONDS	TAI-SECONDS)
	   (UTC-NANOS	(* 1e9 (/ i 10))))
      (check
	  (let* ((TAI (srfi.make-time srfi.time-tai TAI-NANOS TAI-SECONDS))
		 (UTC (srfi.time-tai->time-utc TAI)))
	    (check-pretty-print TAI)
	    (check-pretty-print UTC)
	    UTC)
	(=> srfi.time=?)
	(srfi.make-time srfi.time-utc (exact (floor UTC-NANOS)) UTC-SECONDS))))

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
