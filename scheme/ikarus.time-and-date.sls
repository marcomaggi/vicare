;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of the  GNU General  Public  License version  3  as published  by the  Free
;;;Software Foundation.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.


#!vicare
(library (ikarus time-and-date)
  (export
    ;; time spans
    <time>
    <time>-rtd				<time>-rcd
    <time>-equality-predicate
    <time>-comparison-procedure
    <time>-hash-function
    make-time
    time?				list-of-times?
    time-seconds			time-nanoseconds
    time-ratnum				time-flonum
    time-addition			time-difference
    time=?				time!=?
    time<?				time<=?
    time>?				time>=?
    time-min				time-max

    ;; epoch time
    <epoch-time>
    <epoch-time>-rtd			<epoch-time>-rcd
    <epoch-time>-equality-predicate
    <epoch-time>-comparison-procedure
    <epoch-time>-hash-function
    epoch-time?				list-of-epoch-times?
    current-time
    epoch-time-gmt-offset

    ;; date functions
    date-string
    #| end of EXPORT |# )
  (import (except (vicare)
		  ;; time spans
		  <time>
		  <time>-rtd				<time>-rcd
		  <time>-equality-predicate
		  <time>-comparison-procedure
		  <time>-hash-function
		  make-time
		  time?					list-of-times?
		  time-seconds				time-nanoseconds
		  time-ratnum				time-flonum
		  time-addition				time-difference
		  time=?				time!=?
		  time<?				time<=?
		  time>?				time>=?
		  time-min				time-max

		  ;; epoch time
		  <epoch-time>
		  <epoch-time>-rtd			<epoch-time>-rcd
		  <epoch-time>-equality-predicate
		  <epoch-time>-comparison-procedure
		  <epoch-time>-hash-function
		  epoch-time?				list-of-epoch-times?
		  current-time
		  epoch-time-gmt-offset

		  ;; date functions
		  date-string

		  ;;FIXME  To be  removed at  the next  boot image  rotation.  (Marco
		  ;;Maggi; Mon Sep 19, 2016)
		  compar-fixnum)
    (ikarus records syntactic)
    ;;FIXME To be fixed  at the next boot image rotation.  (Marco  Maggi; Mon Sep 19,
    ;;2016)
    (only (ikarus.comparison-procedures) #;(vicare system comparison-procedures)
	  compar-fixnum)
    (only (vicare language-extensions syntaxes)
	  define-list-of-type-predicate
	  define-min/max-comparison
	  define-equality/sorting-predicate
	  define-inequality-predicate)
    #| end of IMPORT |# )


;;;; base time spans representation

(define-core-record-type <time>
  (nongenerative vicare:core-type:<time>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (fields
    (immutable megasecs)
		;An <exact-integer> representing the megaseconds.
    (immutable secs)
		;A <fixnum> representing the seconds.
    (immutable nanoseconds)
		;A <fixnum> representing the nanoseconds.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-record)
      (define (normalise-triplet megasecs secs nanosecs)
	;;Normalise a triplet: megaseconds, seconds, nanoseconds.
	;;
	(receive (nanos.secs nanos.nanos)
	    (div-and-mod nanosecs #e1e9)
	  (let ((secs (+ secs nanos.secs)))
	    (receive (secs.megas secs.secs)
		(div-and-mod secs #e1e6)
	      (let ((megasecs (+ megasecs secs.megas)))
		(values megasecs secs.secs nanos.nanos))))))

      (case-define* make-<time>
	(({secs exact-integer?} {nanosecs exact-integer?})
	 (make-<time> 0 secs nanosecs))
	(({megasecs exact-integer?} {secs exact-integer?} {nanosecs exact-integer?})
	 (receive (megasecs secs nanosecs)
	     (normalise-triplet megasecs secs nanosecs)
	   (make-record megasecs secs nanosecs))))

      make-<time>))

  (equality-predicate
    (lambda ()
      (lambda/std (T1 T2)
	(and (= (time-megasecs  T1)	(time-megasecs  T2))
	     (= (time-secs      T1)	(time-secs      T2))
	     (= (time-nanoseconds T1)	(time-nanoseconds T2))))))

  (hash-function
    (lambda ()
      (lambda (T)
	;;The megaseconds change rarely.  So we do not use them here.
	(fxior (fixnum-hash (time-secs		T))
	       (fixnum-hash (time-nanoseconds	T))))))

  (comparison-procedure
    (lambda ()
      (lambda (T1 T2)
	(let ((cmp (compar-exact-integer (time-megasecs T1) (time-megasecs T2))))
	  (if (fxzero? cmp)
	      (let ((cmp (compar-fixnum (time-secs T1) (time-secs T2))))
		(if (fxzero? cmp)
		    (compar-fixnum (time-nanoseconds T1) (time-nanoseconds T2))
		  cmp))
	    cmp)))))

  (custom-printer
    (named-lambda <time>-printer (T port subprinter)
      (display "#[<time>" port)
      (display " secs="  port) (display (time-seconds     T) port)
      (display " nsecs=" port) (display (time-nanoseconds T) port)
      (display "]" port)))

  (method (seconds {T time?})
    (+ (* (time-megasecs T) #e1e6)
       (time-secs T)))

  (method (ratnum {T time?})
    (+ (* (time-megasecs T) #e1e6)
       (time-secs T)
       (* (time-nanoseconds T) #e1e-9)))

  (method (flonum {T time?})
    (inexact (time-ratnum T)))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define-list-of-type-predicate list-of-times? time?)


;;;; time operations

(case-define* time-addition
  (({time time?})
   time)
  (({time1 time?} {time2 time?})
   ;;Compute the addition between  two times: time1 + time2 and  return a time object
   ;;representing it.
   ;;
   (make-time (+ (time-megasecs  time1) (time-megasecs  time2))
	      (+ (time-secs      time1) (time-secs      time2))
	      (+ (time-nanoseconds time1) (time-nanoseconds time2))))
  (({T1 time?} {T2 time?} {T3 time?} . {T* time?})
   (fold-left time-addition T1 (cons* T2 T3 T*))))

;;; --------------------------------------------------------------------

(case-define* time-difference
  (({T time?})
   (make-time (- (time-megasecs T))
	      (- (time-secs T))
	      (- (time-nanoseconds T))))
  (({time1 time?} {time2 time?})
   ;;Compute the difference between two times: time1 - time2 and return a time object
   ;;representing it.
   ;;
   (make-time (- (time-megasecs  time1) (time-megasecs  time2))
	      (- (time-secs      time1) (time-secs      time2))
	      (- (time-nanoseconds time1) (time-nanoseconds time2))))
  (({T1 time?} {T2 time?} {T3 time?} . {T* time?})
   (fold-left time-difference T1 (cons* T2 T3 T*))))


;;;; time comparison

(define-equality/sorting-predicate time=?	$time=	time?)
(define-equality/sorting-predicate time<?	$time<	time?)
(define-equality/sorting-predicate time<=?	$time<=	time?)
(define-equality/sorting-predicate time>?	$time>	time?)
(define-equality/sorting-predicate time>=?	$time>=	time?)
(define-inequality-predicate       time!=?	$time!=	time?)

;;; --------------------------------------------------------------------

(define ($time= time1 time2)
  ;;Return true if the time objects are equal, else return false.
  ;;
  (and (= (time-megasecs  time1) (time-megasecs  time2))
       (= (time-secs      time1) (time-secs      time2))
       (= (time-nanoseconds time1) (time-nanoseconds time2))))

(define ($time!= time1 time2)
  ;;Return true if the time objects are different, else return false.
  ;;
  (or (!= (time-megasecs  time1) (time-megasecs  time2))
      (!= (time-secs      time1) (time-secs      time2))
      (!= (time-nanoseconds time1) (time-nanoseconds time2))))

(define ($time< time1 time2)
  ;;Return true if TIME1 is less than TIME2, else return false.
  ;;
  (or (< (time-megasecs time1)
	 (time-megasecs time2))
      (and (= (time-megasecs time1)
	      (time-megasecs time2))
	   (or (< (time-secs time1)
		  (time-secs time2))
	       (and (= (time-secs time1)
		       (time-secs time2))
		    (< (time-nanoseconds time1)
		       (time-nanoseconds time2)))))))

(define ($time<= time1 time2)
  ;;Return true if TIME1 is less than or equal to TIME2, else return false.
  ;;
  (or (< (time-megasecs time1)
	 (time-megasecs time2))
      (and (= (time-megasecs time1)
	      (time-megasecs time2))
	   (or (< (time-secs time1)
		  (time-secs time2))
	       (and (= (time-secs time1)
		       (time-secs time2))
		    (<= (time-nanoseconds time1)
			(time-nanoseconds time2)))))))

(define ($time> time1 time2)
  ;;Return true if TIME1 is greater than TIME2, else return false.
  ;;
  (or (> (time-megasecs time1)
	 (time-megasecs time2))
      (and (= (time-megasecs time1)
	      (time-megasecs time2))
	   (or (> (time-secs time1)
		  (time-secs time2))
	       (and (= (time-secs time1)
		       (time-secs time2))
		    (> (time-nanoseconds time1)
		       (time-nanoseconds time2)))))))

(define ($time>= time1 time2)
  ;;Return true if TIME1 is greater than or equal to TIME2, else return false.
  ;;
  (or (> (time-megasecs time1)
	 (time-megasecs time2))
      (and (= (time-megasecs time1)
	      (time-megasecs time2))
	   (or (> (time-secs time1)
		  (time-secs time2))
	       (and (= (time-secs time1)
		       (time-secs time2))
		    (>= (time-nanoseconds time1)
			(time-nanoseconds time2)))))))


;;;; min max

(define-min/max-comparison time-max $time-max time?)
(define-min/max-comparison time-min $time-min time?)

(define ($time-min T1 T2)
  (if ($time< T1 T2) T1 T2))

(define ($time-max T1 T2)
  (if ($time< T1 T2) T2 T1))


;;;; time since the Epoch

(define-core-record-type <epoch-time>
  (nongenerative vicare:core-types:<epoch-time>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <time>)

  (protocol
    (lambda (make-<time>)
      (named-lambda make-<epoch-time> ()
	(foreign-call "ikrt_current_time" ((make-<time> 0 0 0))))))

  (custom-printer
    (lambda (T port subprinter)
      (display "#[<epoch-time>"	port)
      (display " secs="		port) (display (time-seconds		T) port)
      (display " nsecs="	port) (display (time-nanoseconds	T) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

(define (current-time)
  (make-epoch-time))

(define-list-of-type-predicate list-of-epoch-times? epoch-time?)

(define* (epoch-time-gmt-offset)
  ;;We think of Epoch time as a time point specification in the GMT timezone.  We can
  ;;ask: what is  the offset between GMT  and the timezone configured  as selected by
  ;;the  operating system?   This function  answers  this question  by returning  the
  ;;offset as number of seconds; positive return values mean eastern offset.
  ;;
  ;;A return  value of  3600 means:  we are  in Rome  (GMT+01) and  we must  add 3600
  ;;seconds to the Epoch time to obtain the local time.
  ;;
  (foreign-call "ikrt_gmt_offset"))


;;;; date functions

(define (date-string)
  (let ((s (make-bytevector 10)))
    (foreign-call "ikrt_bvftime" s #ve(ascii "%F"))
    (utf8->string s)))


;;;; done

#| end of library |# )

;;; end of file
