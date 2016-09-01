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
    current-time	time-from-now
    time?
    time-second		time-nanosecond
    time-gmt-offset	date-string

    (rename (make-time-struct make-time))
    time-addition	time-difference
    time=?
    time<?		time<=?
    time>?		time>=?)
  (import (except (vicare)
		  <time>

		  ;;This is the function TIME!!!
		  time

		  current-time		time-from-now
		  time?
		  time-second		time-nanosecond
		  time-gmt-offset	date-string

		  make-time
		  time-addition		time-difference
		  time=?
		  time<?		time<=?
		  time>?		time>=?)
    (vicare system structs))


(define-struct (time %make-time time?)
  (megasecs
		;Exact integer representing the megaseconds.
   secs
		;Exact integer representing the seconds
   microsecs))
		;Exact integer representing the microseconds.

(define* (make-time {megasecs exact-integer?} {secs exact-integer?} {microsecs exact-integer?})
  (%make-time megasecs secs microsecs))

(define (current-time)
  (foreign-call "ikrt_current_time" (make-time 0 0 0)))

(define* (time-from-now {delta time?})
  ($time-addition (current-time) delta))

;;; --------------------------------------------------------------------

(define* (time-second {x time?})
  (+ (* ($time-megasecs x) #e1e6)
     ($time-secs x)))

(define* (time-nanosecond {x time?})
  (* ($time-microsecs x) 1000))

(define* (time-gmt-offset {x time?})
  (foreign-call "ikrt_gmt_offset" x))


;;;; time operations

(define* (make-time-struct {secs exact-integer?} {nanos exact-integer?})
  ($normalise-and-make 0 secs (div nanos 1000)))

(define ($normalise-and-make megas secs micros)
  ;;Normalise  a  triplet:  megaseconds,  seconds, microseconds  and  return  a  TIME
  ;;structure representing the result.
  ;;
  (let-values (((micros.secs micros.micros) (div-and-mod micros #e1e6)))
    (let ((secs (+ secs micros.secs)))
      (let-values (((secs.megas secs.secs) (div-and-mod secs #e1e6)))
	(let ((megas (+ megas secs.megas)))
	  (make-time megas secs.secs micros.micros))))))

;;; --------------------------------------------------------------------

(define* (time-addition {time1 time?} {time2 time?})
  ;;Compute the addition  between two times: time1  - time2 and return  a time struct
  ;;representing it.
  ;;
  ($time-addition time1 time2))

(define ($time-addition time1 time2)
  ($normalise-and-make (+ ($time-megasecs  time1) ($time-megasecs  time2))
		       (+ ($time-secs      time1) ($time-secs      time2))
		       (+ ($time-microsecs time1) ($time-microsecs time2))))

;;; --------------------------------------------------------------------

(define* (time-difference {time1 time?} {time2 time?})
  ;;Compute the difference between two times: time1  - time2 and return a time struct
  ;;representing it.
  ;;
  ($time-difference time1 time2))

(define ($time-difference time1 time2)
  ($normalise-and-make (- ($time-megasecs  time1) ($time-megasecs  time2))
		       (- ($time-secs      time1) ($time-secs      time2))
		       (- ($time-microsecs time1) ($time-microsecs time2))))


;;;; time comparison

(define* (time=? {time1 time?} {time2 time?})
  ;;Return true if the time objects are equal, else return false.
  ;;
  ($time=? time1 time2))

(define ($time=? time1 time2)
  (and (= ($time-megasecs  time1) ($time-megasecs  time2))
       (= ($time-secs      time1) ($time-secs      time2))
       (= ($time-microsecs time1) ($time-microsecs time2))))

;;; --------------------------------------------------------------------

(define* (time<? {time1 time?} {time2 time?})
  ;;Return true if TIME1 is less than TIME2, else return false.
  ;;
  ($time<? time1 time2))

(define ($time<? time1 time2)
  (or (< ($time-megasecs time1)
	 ($time-megasecs time2))
      (and (= ($time-megasecs time1)
	      ($time-megasecs time2))
	   (or (< ($time-secs time1)
		  ($time-secs time2))
	       (and (= ($time-secs time1)
		       ($time-secs time2))
		    (< ($time-microsecs time1)
		       ($time-microsecs time2)))))))

;;; --------------------------------------------------------------------

(define* (time<=? {time1 time?} {time2 time?})
  ;;Return true if TIME1 is less than or equal to TIME2, else return false.
  ;;
  ($time<=? time1 time2))

(define ($time<=? time1 time2)
  (or (< ($time-megasecs time1)
	 ($time-megasecs time2))
      (and (= ($time-megasecs time1)
	      ($time-megasecs time2))
	   (or (< ($time-secs time1)
		  ($time-secs time2))
	       (and (= ($time-secs time1)
		       ($time-secs time2))
		    (<= ($time-microsecs time1)
			($time-microsecs time2)))))))

;;; --------------------------------------------------------------------

(define* (time>? {time1 time?} {time2 time?})
  ;;Return true if TIME1 is greater than TIME2, else return false.
  ;;
  ($time>? time1 time2))

(define ($time>? time1 time2)
  (or (> ($time-megasecs time1)
	 ($time-megasecs time2))
      (and (= ($time-megasecs time1)
	      ($time-megasecs time2))
	   (or (> ($time-secs time1)
		  ($time-secs time2))
	       (and (= ($time-secs time1)
		       ($time-secs time2))
		    (> ($time-microsecs time1)
		       ($time-microsecs time2)))))))

;;; --------------------------------------------------------------------

(define* (time>=? {time1 time?} {time2 time?})
  ;;Return true if TIME1 is greater than or equal to TIME2, else return false.
  ;;
  ($time>=? time1 time2))

(define ($time>=? time1 time2)
  (or (> ($time-megasecs time1)
	 ($time-megasecs time2))
      (and (= ($time-megasecs time1)
	      ($time-megasecs time2))
	   (or (> ($time-secs time1)
		  ($time-secs time2))
	       (and (= ($time-secs time1)
		       ($time-secs time2))
		    (>= ($time-microsecs time1)
			($time-microsecs time2)))))))


;;;; date functions

(define (date-string)
  (let ((s (make-bytevector 10)))
    (foreign-call "ikrt_bvftime" s (string->utf8 "%F"))
    (utf8->string s)))


;;;; done

#| end of library |# )

;;; end of file
