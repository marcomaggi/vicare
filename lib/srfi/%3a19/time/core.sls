;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: extensions to SRFI 19, time functions
;;;Date: Wed Jan 23, 2013
;;;
;;;Abstract
;;;
;;;	This  library is  derived from  the reference  implementation of
;;;	SRFI 19.  There  are some changes in the  handling of conversion
;;;	between the  UTC and TAI scales.   The result is that,  for some
;;;	input  values,  the  results  computed  with  this  library  are
;;;	different   from   the   ones  computed   with   the   reference
;;;	implementation of SRFI 19.
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


;;;; conventions
;;
;;When expressing a time point  in this documentation, we preferably use
;;strings of the following format, which is compliant with ISO 8601:
;;
;;   <YYYY>-<MM>-<DD>T<hh>:<mm>:<ss>Z
;;
;;where:
;;
;;* <YYYY> is the year number;
;;
;;* <MM> is the month index in  the range [1, 12];
;;
;;* <DD> is  the day index in  the range [1, 31];
;;
;;* <hh> is the count of hours in the range [0, 23];
;;
;;* <mm> is the count of minutes in the range [0, 59];
;;
;;* <ss>  is the count  of seconds in the  range [0, 60]  (allowing leap
;;  seconds);
;;
;;* the literal "T" separates the date from the hour;
;;
;;* the literal "Z" specifies the mean time of Greenwich (UK).
;;
;;The beginning of  the 1st second in  a day is 00:00:00 and  we call it
;;"midnight"; the beginning of the last second in a day is 23:59:59 when
;;no leap second is added and 23:59:60 when a leap second is added.
;;


;;;; terrestrial time
;;
;; <http://en.wikipedia.org/wiki/Terrestrial_Time>
;;
;;When using the this library we  aim at dealing with "Terrestrial Time"
;;in which:  seconds are the ones  of the International System  of Units
;;(SI), defined using caesium atomic clocks; 60 seconds make one minute;
;;60 minutes  make one hour;  24 hours make one  day; 365 days  make one
;;year.  Terrestrial Time is a  theoretical ideal, which real clocks can
;;only approximate.
;;


;;;; international atomic time
;;
;; <http://en.wikipedia.org/wiki/International_Atomic_Time>
;;
;;A realisation  of Terrestrial  Time is  the International  Atomic Time
;;(TAI),  which is  based  on the  notional passage  of  proper time  on
;;Earth's surface.
;;
;;Time coordinates on  the TAI scale are expressed using  the SI seconds
;;and the time coordinates defined by the proleptic Gregorian calendar.
;;
;;The timestamp 2010-08-05T11:22:33Z is valid  on the TAI scale with the
;;usual count of years, months and  days on the Gregorian calendar.  The
;;count of seconds is always in the  range [0, 59] because the TAI scale
;;has no leap seconds: every TAI day has 86400 seconds.
;;


;;;; coordinated universal time
;;
;; <http://en.wikipedia.org/wiki/Coordinated_Universal_Time>
;; <http://en.wikipedia.org/wiki/Leap_second>
;;
;;Another realisation  of Terrestrial Time is  the Coordinated Universal
;;Time (UTC),  a time standard based  on TAI with leap  seconds added at
;;irregular intervals  to compensate  for the Earth's  slowing rotation.
;;Leap seconds are added at the end of some days to allow UTC to closely
;;track the  mean solar time  at the Royal Observatory,  Greenwich (UK);
;;when  a leap  second is  added:  the last  minute  of the  day has  61
;;seconds.
;;
;;The ISO 8601 timestamp 2010-08-05T11:22:33Z  is valid on the UTC scale
;;with  the usual  count  of years,  months and  days  on the  Gregorian
;;calendar.  The count of seconds is usually in the range [0, 59] but it
;;is in the range [0, 60] on the specific days in which a leap second is
;;added.
;;
;;The  first single  leap  second was  added  on June  30  1972, so  the
;;timestamp 1972-06-30T23:59:60Z is  valid on the UTC  scale but invalid
;;on the TAI scale.
;;
;;All the timestamps  with seconds count in the range  [0, 59] are valid
;;on both  the TAI scale and  the UTC scale; the  difference between the
;;two scales shows only on the specific timestamps representing the leap
;;seconds and when converting a timestamp from the Gregorian calendar to
;;a count  of seconds since  some conventional origin  (examples: Julian
;;Date, time since the Unix Epoch).
;;
;;As a compromise between the history  of time scale definitions and the
;;need to define  a time scale for  every point in time,  we assume that
;;the  UTC  and  TAI  scales   are  synchronised  up  to  and  including
;;1971-12-31T23:59:59Z;  so,  for  the  immediate  seconds  before  such
;;timestamp, we can write:
;;
;;   1971-12-31T23:59:57Z[UTC] = 1971-12-31T23:59:57Z[TAI]
;;   1971-12-31T23:59:58Z[UTC] = 1971-12-31T23:59:58Z[TAI]
;;   1971-12-31T23:59:59Z[UTC] = 1971-12-31T23:59:59Z[TAI]
;;
;;but from the UTC timestamp (included) 1972-01-01T00:00:00Z onwards and
;;up  to  the  first  leap  second (included):  the  UTC  scale  is,  by
;;definition, 10 seconds  behind the TAI scale  (such initial difference
;;between UTC and TAI is not a proper leap second); so:
;;
;;   1972-01-01T00:00:00Z[UTC] = 1972-01-01T00:00:10Z[TAI]
;;
;;We assume  that the  UTC second between  1971-12-31T23:59:59Z[UTC] and
;;1971-01-01T00:00:00Z[UTC]  is 11  seconds long  on the  TAI scale,  as
;;follows:
;;
;;                                   past
;;                                    |
;;       -- 1971-12-31T23:59:59Z[UTC] + 1971-12-31T23:59:59Z[TAI]  --
;;        |                           |                            |  1
;;        |                           + 1971-01-01T00:00:00Z[TAI]  +-
;;        |                           |                            |  2
;;        |                           + 1971-01-01T00:00:01Z[TAI]  +-
;;        |                           |                            |  3
;;        |                           + 1971-01-01T00:00:02Z[TAI]  +-
;;        |                           |                            |  4
;;        |                           + 1971-01-01T00:00:03Z[TAI]  +-
;;        |                           |                            |  5
;; bizarre|                           + 1971-01-01T00:00:04Z[TAI]  +-
;;   UTC  |                           |                            |  6
;; second |                           + 1971-01-01T00:00:05Z[TAI]  +-
;;        |                           |                            |  7
;;        |                           + 1971-01-01T00:00:06Z[TAI]  +-
;;        |                           |                            |  8
;;        |                           + 1971-01-01T00:00:07Z[TAI]  +-
;;        |                           |                            |  9
;;        |                           + 1971-01-01T00:00:08Z[TAI]  +-
;;        |                           |                            | 10
;;        |                           + 1971-01-01T00:00:09Z[TAI]  +-
;;        |                           |                            | 11
;;       -- 1971-01-01T00:00:00Z[UTC] + 1971-01-01T00:00:10Z[TAI]  --
;;                                    |
;;                                 future
;;
;;such  elongated UTC  second  is  called "bizarre  UTC  second" in  the
;;context of this library.
;;
;;When the 1st leap second was added, on June 30, 1972: TAI and UTC went
;;further  out of  synchronisation, from  10 seconds  to 11  seconds, as
;;follows:
;;
;;        UTC 10 seconds behind TAI | UTC 11 seconds behind TAI
;;
;;        00:00:08 00:00:09 00:00:10 00:00:11 00:00:12 00:00:13
;; TAI --|--------|--------|--------|--------|--------|--------|--
;;
;;        23:59:58 23:59:59 23:59:60 00:00:00 00:00:01 00:00:02
;; UTC --|--------|--------|++++++++|--------|--------|--------|--
;;                           leap   ^
;;                           second |
;;                                  |
;;                    June 30 1972  |  July 1 1972
;;                                  |
;;                            UTC midnight
;;
;;and as follows:
;;
;;                                  past
;;                                    |
;;          1972-06-30T23:59:58Z[UTC] + 1972-07-01T00:00:08Z[TAI]
;;                                    |
;;          1972-06-30T23:59:59Z[UTC] + 1972-07-01T00:00:09Z[TAI]
;;                                    |
;;       -- 1972-06-30T23:59:60Z[UTC] + 1972-07-01T00:00:10Z[TAI]
;;   leap |                           |
;;       -- 1972-07-01T00:00:00Z[UTC] + 1972-07-01T00:00:11Z[TAI]
;;                                    |
;;          1972-07-01T00:00:01Z[UTC] + 1972-07-01T00:00:12Z[TAI]
;;                                    |
;;                                 future
;;
;;notice that there  is *no* elongated second when a  proper leap second
;;is added and we represent the time with ISO 8601 timestamps.
;;
;;When the 2nd leap second was added,  on December 31, 1972: TAI and UTC
;;went further out of synchronisation, from 11 seconds to 12 seconds, as
;;follows:
;;
;;          UTC 11 seconds behind TAI | UTC 12 seconds behind TAI
;;
;;          00:00:09 00:00:10 00:00:11 00:00:12 00:00:13 00:00:14
;;   TAI --|--------|--------|--------|--------|--------|--------|--
;;
;;          23:59:58 23:59:59 23:59:60 00:00:00 00:00:01 00:00:02
;;   UTC --|--------|--------|++++++++|--------|--------|--------|--
;;                             leap   ^
;;                             second |
;;                                    |
;;                  December 31 1972  |  January 1 1973
;;                                    |
;;                              UTC midnight
;;
;;and as follows:
;;
;;                                  past
;;                                    |
;;          1972-12-31T23:59:58Z[UTC] + 1973-01-01T00:00:09Z[TAI]
;;                                    |
;;          1972-12-31T23:59:59Z[UTC] + 1973-01-01T00:00:10Z[TAI]
;;                                    |
;;       -- 1972-12-31T23:59:60Z[UTC] + 1973-01-01T00:00:11Z[TAI]
;;   leap |                           |
;;       -- 1973-01-01T00:00:00Z[UTC] + 1973-01-01T00:00:12Z[TAI]
;;                                    |
;;          1973-01-01T00:00:01Z[UTC] + 1973-01-01T00:00:13Z[TAI]
;;                                    |
;;                                 future
;;
;;The same happens for the other leap  seconds.  As of Jan 23, 2013: the
;;last leap second was added  on 2012-06-30T23:59:60Z, after with UTC is
;;35 seconds behind TAI.
;;
;;It is possible to compute the  exact time interval elapsed between two
;;UTC timestamps only by consulting a table that describes how many leap
;;seconds occurred during that interval.
;;
;;                        |  TAI - UTC |
;;      UTC timestamp     | after leap |    Description
;;  ----------------------+------------+-------------------
;;   1970-01-01T00:00:00Z |      0     | Unix Epoch
;;   1971-12-31T23:59:59Z |      0     | bizarre UTC second
;;   1972-01-01T00:00:00Z |     10     | initial delta
;;   1972-06-30T23:59:60Z |     11     |  1st leap second
;;   1972-12-31T23:59:60Z |     12     |  2nd leap second
;;   1973-12-31T23:59:60Z |     13     |  3rd leap second
;;   1974-12-31T23:59:60Z |     14     |  4th leap second
;;   1975-12-31T23:59:60Z |     15     |  5th leap second
;;   1976-12-31T23:59:60Z |     16     |  6th leap second
;;   1977-12-31T23:59:60Z |     17     |  7th leap second
;;   1978-12-31T23:59:60Z |     18     |  8th leap second
;;   1979-12-31T23:59:60Z |     19     |  9th leap second
;;   1981-06-30T23:59:60Z |     20     | 10th leap second
;;   1982-06-30T23:59:60Z |     21     | 11th leap second
;;   1983-06-30T23:59:60Z |     22     | 12th leap second
;;   1985-06-30T23:59:60Z |     23     | 13th leap second
;;   1987-12-31T23:59:60Z |     24     | 14th leap second
;;   1989-12-31T23:59:60Z |     25     | 15th leap second
;;   1990-12-31T23:59:60Z |     26     | 16th leap second
;;   1992-06-30T23:59:60Z |     27     | 17th leap second
;;   1993-06-30T23:59:60Z |     28     | 18th leap second
;;   1994-06-30T23:59:60Z |     29     | 19th leap second
;;   1995-12-31T23:59:60Z |     30     | 20th leap second
;;   1997-06-30T23:59:60Z |     31     | 21th leap second
;;   1998-12-31T23:59:60Z |     32     | 22th leap second
;;   2005-12-31T23:59:60Z |     33     | 23th leap second
;;   2008-12-31T23:59:60Z |     34     | 24th leap second
;;   2012-06-30T23:59:60Z |     35     | 25th leap second
;;


;;;; count of seconds since the Unix Epoch
;;
;; <http://en.wikipedia.org/wiki/Unix_time>
;;
;;When manipulating time  with computers, it is  useful and historically
;;important to  express points  in time  with the  count of  seconds and
;;nanoseconds since a special (and arbitrary) origin: the Unix Epoch.
;;
;;The Unix  Time, or POSIX  Time, is a  system for describing  points in
;;time,   defined    as   the   number   of    seconds   elapsed   since
;;1970-01-01T00:00:00Z,  not  counting  leap  seconds;  this  artificial
;;origin for time points is called "Unix Epoch".  In the context of this
;;library we  call such count  of seconds "Epoch timestamp",  or "Etime"
;;for short.
;;
;;There is no problem at all in defining the Epoch timestamps on the TAI
;;scale: TAI  uses SI  seconds and  no leap seconds,  so the  TAI offset
;;since the Epoch  is just a linear function.  By  consulting a table of
;;leap seconds we  can convert a TAI  Epoch timestamp to a  UTC ISO 8601
;;timestamp with few  problems (using the bizarre UTC second  if we know
;;the fraction of a second).
;;
;;Problems arise  if we want to  define the Epoch timestamps  on the UTC
;;scale: do we want to define the  UTC Epoch timestamps to be behind the
;;TAI Epoch timestamps  as the ISO 8601 timestamps are?   In the context
;;of this library we decide that: yes, we do.
;;
;;Weird things  happen in defining  such Epoch timestamps, so  let's see
;;the process in detail.  The Unix Epoch is:
;;
;;   1970-01-01T00:00:00Z[UTC]		UTC Etime = 0
;;   1970-01-01T00:00:00Z[TAI]		TAI Etime = 0
;;
;;It is decided  that from -inf.0 (in the  proleptic Gregorian calendar)
;;up to the ISO 8601 timestamp (included):
;;
;;   1971-12-31T23:59:59Z[UTC]	UTC Etime = 63072000 - 1 = 63071999
;;   1971-12-31T23:59:59Z[TAI]	TAI Etime = 63072000 - 1 = 63071999
;;
;;the UTC scale is synchronised with the TAI scale:
;;
;;   UTC scale    past     TAI scale
;;                 |
;;        63071997 + 63071997
;;                 |
;;        63071998 + 63071998
;;                 |
;;        63071999 + 63071999
;;                 |
;;              future
;;
;;but from the time point (included):
;;
;;   1972-01-01T00:00:00Z[UTC] = 1972-01-01T00:00:10Z[TAI]
;;
;;onwards and up to the 1st leap  second (included): the UTC scale is 10
;;seconds behind the TAI scale:
;;
;;   UTC scale    past   TAI scale
;;                 |
;;        63072000 + 63072010
;;                 |
;;        63072001 + 63072011
;;                 |
;;        63072002 + 63072012
;;                 |
;;              future
;;
;;Notice   how    we   compute    the   number   of    seconds   between
;;1970-01-01T00:00:00Z[TAI] and 1972-01-01T00:00:00Z[TAI]:
;;
;;   NUM-OF-YEARS       = 1972 - 1970 = 2
;;   DAYS-IN-ONE-YEAR   = 365
;;   SECONDS-IN-ONE-DAY = 60 * 60 * 24 = 86400
;;   SECONDS-IN-ONE-DAY * DAYS-IN-ONE-YEAR * NUM-OF-YEARS = 63072000
;;
;;So, given the two equalities:
;;
;;   63071999[UTC] = 63071999[TAI]
;;   63072000[UTC] = 63072010[TAI]
;;
;;in this  library we assume  that the UTC second  between 63071999[UTC]
;;and 63072000[UTC] is 11 seconds long on the TAI scale, as follows:
;;
;;    63072004 -----------------   ----------------- 63072005
;;    63072003 ---------------  | |  --------------- 63072006
;;    63072002 -------------  | | | |  ------------- 63072007
;;    63072001 -----------  | | | | | |  ----------- 63072008
;;    63072000 ---------  | | | | | | | |  --------- 63072009
;;    63072000-1 -----  | | | | | | | | | |  ------- 63072010
;;    63072000-2 ---  | | | | | | | | | | | |  ----- 63072011
;;    63072000-3 -  | | | | | | | | | | | | | |  --- 63072012
;;                | | | | | | | | | | | | | | | |
;;   -------------+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+--------- TAI seconds
;;   -------------+-+-+---------------------+-+-+--------- UTC seconds
;;                | | |                     | | |
;;    63072000-3 -  | |                     | |  --- 63072002
;;    63072000-2 ---  |                     |  ----- 63072001
;;    63072000-1 -----                       ------- 63072000
;;
;;                    |.|.|.|.|.|.|.|.|.|.|.| 11 TAI seconds
;;                    |.....................| bizarre UTC second
;;
;;    |-| 1 TAI second
;;    |-| 1 UTC second (except the bizarre UTC second)
;;
;;such  elongated UTC  second  is  called "bizarre  UTC  second" in  the
;;context of this library.
;;
;;The beginning of the 1st leap second has timestamp:
;;
;;   1972-06-30T23:59:60Z[UTC] = 1972-07-01T00:00:10Z[TAI]
;;
;;and when it happens time flows as follows on the UTC and TAI scales:
;;
;;                                    past
;;                                      |
;;            1972-06-30T23:59:58Z[UTC] + 1972-07-01T00:00:08Z[TAI]
;;                                      |
;;            1972-06-30T23:59:59Z[UTC] + 1972-07-01T00:00:09Z[TAI]
;;                                      |
;;   leap   - 1972-06-30T23:59:60Z[UTC] + 1972-07-01T00:00:10Z[TAI]
;;   second |                           |
;;          - 1972-07-01T00:00:00Z[UTC] + 1972-07-01T00:00:11Z[TAI]
;;                                      |
;;            1972-07-01T00:00:01Z[UTC] + 1972-07-01T00:00:12Z[TAI]
;;                                      |
;;                                   future
;;
;;so inside  the leap  second UTC  time is still  10 seconds  behind TAI
;;time, but from the  end of the leap second onwards up  to the 2nd leap
;;second (included): the UTC scale is 11 seconds behind the TAI scale.
;;
;;If we just write the Epoch timestamps counting second by second around
;;the leap second, the result is:
;;
;;              78796797[UTC] = 78796807[TAI]	10 seconds delta
;;              78796798[UTC] = 78796808[TAI]	10 seconds delta
;; leap second  78796799[UTC] = 78796809[TAI]	10 seconds delta
;;              78796810[UTC] = 78796810[TAI]	10 seconds delta
;;              78796800[UTC] = 78796811[TAI]	10 seconds delta
;;
;;and  the delta  between UTC  and  TAI does  not change.   But we  have
;;decided to  define UTC  Epoch timestamps  to be  behind the  TAI Epoch
;;timestamps like  the ISO 8601  timestamps are,  so the insertion  of a
;;leap second on the UTC scale must delay the UTC scale of 1 second with
;;respect to the TAI scale; so  this is the sequence of Epoch timestamps
;;we need:
;;
;;              78796797[UTC] = 78796807[TAI]	10 seconds delta
;;              78796798[UTC] = 78796808[TAI]	10 seconds delta
;; leap second  78796799[UTC] = 78796809[TAI]	10 seconds delta
;;                              78796810[TAI]
;;              78796800[UTC] = 78796811[TAI]	11 seconds delta
;;
;;we have a hole to fill!!!  There are multiple ways to fill it:
;;
;;1. We might repeat twice the count 78796799[UTC], as follows:
;;
;;                             past
;;                               |
;;                 78796797[UTC] + 78796807[TAI]   10 seconds delta
;;                               |
;;                 78796798[UTC] + 78796808[TAI]   10 seconds delta
;;                               |
;;             --  78796799[UTC] + 78796809[TAI]   10 seconds delta
;;       leap   |                |
;;             --  78796799[UTC] + 78796810[TAI]   10 seconds delta
;;       leap   |                |
;;             --  78796800[UTC] + 78796811[TAI]   11 seconds delta
;;                               |
;;                            future
;;
;;   in which  case there are  two leap seconds, somewhat;  time repeats
;;   itself.  We cannot distinguish the first leap from the second leap.
;;
;;2. We might elongate  the UTC leap second so that it  is 2 TAI seconds
;;   long, as follows:
;;
;;                             past
;;                               |
;;                 78796797[UTC] + 78796807[TAI]   10 seconds delta
;;                               |
;;                 78796798[UTC] + 78796808[TAI]   10 seconds delta
;;                               |
;;             --  78796799[UTC] + 78796809[TAI]   10 seconds delta
;;       leap   |                |
;;       second |                + 78796810[TAI]   10.5 seconds delta
;;              |                |
;;             --  78796800[UTC] + 78796811[TAI]   11 seconds delta
;;                               |
;;                            future
;;
;;in this library we choose solution 2.
;;

;;The following table associates:
;;
;;* The UTC ISO 8601 timestamps of  the beginning of leap seconds.
;;
;;* The UTC Epoch timestamps of the beginning of the seconds 1 after the
;;  leap second.
;;
;;* The TAI Epoch timestamps of the beginning of the seconds 2 after the
;;  leap second.
;;
;;For example,  for the 1st leap  second the ISO 8601  timestamps of the
;;leap second are:
;;
;;  1972-06-30T23:59:60Z[UTC] = 1972-06-30T00:00:10[TAI]
;;
;;the Epoch timestamps of the leap second are:
;;
;;     (78796800-1)[UTC] = (78796811-2)[TAI]
;;
;;and the Epoch timestamps of the second after the leap second are:
;;
;;     78796800[UTC] = 78796811[TAI]
;;
;;----------------------+-------------+-------------+------------------
;;    UTC ISO 8601      | UTC Etime   | TAI Etime   |
;;    timestamp         | 1 sec after | 2 sec after |  Description
;;----------------------+-------------+-------------+------------------
;; 1972-06-30T23:59:60Z |   78796800  |   78796811  |  1st leap second
;; 1972-12-31T23:59:60Z |   94694400  |   94694412  |  2nd leap second
;; 1973-12-31T23:59:60Z |  126230400  |  126230413  |  3rd leap second
;; 1974-12-31T23:59:60Z |  157766400  |  157766414  |  4th leap second
;; 1975-12-31T23:59:60Z |  189302400  |  189302415  |  5th leap second
;; 1976-12-31T23:59:60Z |  220924800  |  220924816  |  6th leap second
;; 1977-12-31T23:59:60Z |  252460800  |  252460817  |  7th leap second
;; 1978-12-31T23:59:60Z |  283996800  |  283996818  |  8th leap second
;; 1979-12-31T23:59:60Z |  315532800  |  315532819  |  9th leap second
;; 1981-06-30T23:59:60Z |  362793600  |  362793620  | 10th leap second
;; 1982-06-30T23:59:60Z |  394329600  |  394329621  | 11th leap second
;; 1983-06-30T23:59:60Z |  425865600  |  425865622  | 12th leap second
;; 1985-06-30T23:59:60Z |  489024000  |  489024023  | 13th leap second
;; 1987-12-31T23:59:60Z |  567993600  |  567993624  | 14th leap second
;; 1989-12-31T23:59:60Z |  631152000  |  631152025  | 15th leap second
;; 1990-12-31T23:59:60Z |  662688000  |  662688026  | 16th leap second
;; 1992-06-30T23:59:60Z |  709948800  |  709948827  | 17th leap second
;; 1993-06-30T23:59:60Z |  741484800  |  741484828  | 18th leap second
;; 1994-06-30T23:59:60Z |  773020800  |  773020829  | 19th leap second
;; 1995-12-31T23:59:60Z |  820454400  |  820454430  | 20th leap second
;; 1997-06-30T23:59:60Z |  867715200  |  867715231  | 21th leap second
;; 1998-12-31T23:59:60Z |  915148800  |  915148832  | 22th leap second
;; 2005-12-31T23:59:60Z | 1136073600  | 1136073633  | 23th leap second
;; 2008-12-31T23:59:60Z | 1230768000  | 1230768034  | 24th leap second
;; 2012-06-30T23:59:60Z | 1341100800  | 1341100835  | 25th leap second
;;----------------------+-------------+-------------+------------------
;;
;;Another useful table is the one associating:
;;
;;* The UTC ISO 8601 timestamps of the beginning of leap seconds.
;;
;;* The UTC Epoch timestamps of the beginning of the seconds 1 after the
;;  leap second.
;;
;;* The difference TAI Etime - UTC Etime of the seconds 1 after the leap
;;  second.
;;
;;----------------------+-------------+-------------+------------------
;;    UTC ISO 8601      | UTC Etime   | TAI - UTC   |
;;    timestamp         | 1 sec after | 1 sec after |  Description
;;----------------------+-------------+-------------+------------------
;; 1972-06-30T23:59:60Z |   78796800  |      11     |  1st leap second
;; 1972-12-31T23:59:60Z |   94694400  |      12     |  2nd leap second
;; 1973-12-31T23:59:60Z |  126230400  |      13     |  3rd leap second
;; 1974-12-31T23:59:60Z |  157766400  |      14     |  4th leap second
;; 1975-12-31T23:59:60Z |  189302400  |      15     |  5th leap second
;; 1976-12-31T23:59:60Z |  220924800  |      16     |  6th leap second
;; 1977-12-31T23:59:60Z |  252460800  |      17     |  7th leap second
;; 1978-12-31T23:59:60Z |  283996800  |      18     |  8th leap second
;; 1979-12-31T23:59:60Z |  315532800  |      19     |  9th leap second
;; 1981-06-30T23:59:60Z |  362793600  |      20     | 10th leap second
;; 1982-06-30T23:59:60Z |  394329600  |      21     | 11th leap second
;; 1983-06-30T23:59:60Z |  425865600  |      22     | 12th leap second
;; 1985-06-30T23:59:60Z |  489024000  |      23     | 13th leap second
;; 1987-12-31T23:59:60Z |  567993600  |      24     | 14th leap second
;; 1989-12-31T23:59:60Z |  631152000  |      25     | 15th leap second
;; 1990-12-31T23:59:60Z |  662688000  |      26     | 16th leap second
;; 1992-06-30T23:59:60Z |  709948800  |      27     | 17th leap second
;; 1993-06-30T23:59:60Z |  741484800  |      28     | 18th leap second
;; 1994-06-30T23:59:60Z |  773020800  |      29     | 19th leap second
;; 1995-12-31T23:59:60Z |  820454400  |      30     | 20th leap second
;; 1997-06-30T23:59:60Z |  867715200  |      31     | 21th leap second
;; 1998-12-31T23:59:60Z |  915148800  |      32     | 22th leap second
;; 2005-12-31T23:59:60Z | 1136073600  |      33     | 23th leap second
;; 2008-12-31T23:59:60Z | 1230768000  |      34     | 24th leap second
;; 2012-06-30T23:59:60Z | 1341100800  |      35     | 25th leap second
;;----------------------+-------------+-------------+------------------
;;
;;This table of leap seconds can be automatically built with a data file
;;from:
;;
;;   <ftp://maia.usno.navy.mil/ser7/tai-utc.dat>
;;

;;Given  the table  of  leap seconds,  and knowing  that  there are  1e9
;;nanoseconds in  1 second, we  can convert a UTC  Etime to a  TAI Etime
;;count as follows:
;;
;;   # Before the initial delta.
;;   if utc-seconds < (63072000 - 1)
;;   then
;;      tai-seconds     = utc-seconds
;;      tai-nanoseconds = utc-nanoseconds
;;
;;   # Inside the initial delta.
;;   else if utc-seconds = 63072000 - 1
;;   then
;;      nanos = 11 * utc-nanoseconds
;;      tai-seconds     = (63072000 - 1) + div(nanos, 1e9)
;;      tai-nanoseconds = mod(nanos, 1e9)
;;
;;   # After the initial delta, before the 1st leap second.
;;   else if 63072000 <= utc-seconds < (78796800 - 1)
;;   then
;;      tai-seconds     = utc-seconds + 10
;;      tai-nanoseconds = utc-nanoseconds
;;
;;   # Inside the 1st leap second.
;;   else if utc-seconds = (78796800 - 1)
;;   then
;;      nanos           = 2 * utc-nanoseconds
;;      delta[1st]      = 11
;;      tai-seconds     = (78796800 + delta[1st] - 2) + div(nanos, 1e9)
;;      tai-nanoseconds = mod(nanos, 1e9)
;;
;;   # After the 1st leap second, before the 2nd leap second.
;;   else if 78796800 <= utc-seconds < (94694400 - 1)
;;   then
;;      delta[1st]      = 11
;;      tai-seconds     = utc-seconds + delta[1st]
;;      tai-nanoseconds = utc-nanoseconds
;;
;;   # Inside the 2nd leap second.
;;   else if utc-seconds = (94694400 - 1)
;;   then
;;      nanos           = 2 * utc-nanoseconds
;;      delta[2nd]      = 12
;;      tai-seconds     = (94694400 + delta[2nd] - 2) + div(nanos, 1e9)
;;      tai-nanoseconds = mod(nanos, 1e9)
;;
;;   # After the 2nd leap second, before the 3rd leap second.
;;   else if 94694400 <= utc-seconds < (126230400 - 1)
;;   then
;;      delta[2nd]      = 12
;;      tai-seconds     = utc-seconds + delta[2nd]
;;      tai-nanoseconds = utc-nanoseconds
;;
;;   else ...
;;


;;;; leap seconds layout

;;; Initial delta between UTC and TAI.
;;
;;UTC IS 8601 timestamps:            past
;;                                    |
;;       -- 1971-12-31T23:59:59Z[UTC] + 1971-12-31T23:59:59Z[TAI]  --
;;        |                           |                            |  1
;;        |                           + 1971-01-01T00:00:00Z[TAI]  +-
;;        |                           |                            |  2
;;        |                           + 1971-01-01T00:00:01Z[TAI]  +-
;;        |                           |                            |  3
;;        |                           + 1971-01-01T00:00:02Z[TAI]  +-
;;        |                           |                            |  4
;;        |                           + 1971-01-01T00:00:03Z[TAI]  +-
;;        |                           |                            |  5
;; bizarre|                           + 1971-01-01T00:00:04Z[TAI]  +-
;;   UTC  |                           |                            |  6
;; second |                           + 1971-01-01T00:00:05Z[TAI]  +-
;;        |                           |                            |  7
;;        |                           + 1971-01-01T00:00:06Z[TAI]  +-
;;        |                           |                            |  8
;;        |                           + 1971-01-01T00:00:07Z[TAI]  +-
;;        |                           |                            |  9
;;        |                           + 1971-01-01T00:00:08Z[TAI]  +-
;;        |                           |                            | 10
;;        |                           + 1971-01-01T00:00:09Z[TAI]  +-
;;        |                           |                            | 11
;;       -- 1971-01-01T00:00:00Z[UTC] + 1971-01-01T00:00:10Z[TAI]  --
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                      63071997[UTC] + 63071997[TAI]
;;                                    |
;;                      63071998[UTC] + 63071998[TAI]
;;                                    |
;;                      63071999[UTC] + 63071999[TAI]
;;                                    |
;;                                    + 63072000[TAI]
;;                                    |
;;                                    + 63072001[TAI]
;;                                    |
;;                                    + 63072002[TAI]
;;                                    |
;;                                    + 63072003[TAI]
;;                                    |
;;                                    + 63072004[TAI]
;;                                    |
;;                                    + 63072005[TAI]
;;                                    |
;;                                    + 63072006[TAI]
;;                                    |
;;                                    + 63072007[TAI]
;;                                    |
;;                                    + 63072008[TAI]
;;                                    |
;;                                    + 63072009[TAI]
;;                                    |
;;                      63072000[UTC] + 63072010[TAI]
;;                                    |
;;                                 future
;;

;;; --------------------------------------------------------------------
;;; 1st leap second
;;
;;Leap second UTC timestamp: 1972-06-30T23:59:60Z
;;After leap second UTC is behind TAI by: 11 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1972-06-30T23:59:59Z[UTC] + 1972-07-01T00:00:09Z[TAI]
;;                                    |
;;  leap    1972-06-30T23:59:60Z[UTC] + 1972-07-01T00:00:10Z[TAI]
;;                                    |
;;          1972-07-01T00:00:00Z[UTC] + 1972-07-01T00:00:11Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                   -- 78796799[UTC] + 78796809[TAI] --   delay 10
;;                    |               |               | 1
;;              leap  |               + 78796810[TAI] --   delay 10.5
;;                    |               |               | 2
;;                   -- 78796800[UTC] + 78796811[TAI] --   delay 11
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 2nd leap second
;;
;;Leap second UTC timestamp: 1972-12-31T23:59:60Z
;;After leap second UTC is behind TAI by: 12 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1972-12-31T23:59:59Z[UTC] + 1973-01-01T00:00:10Z[TAI]
;;                                    |
;;  leap    1972-12-31T23:59:60Z[UTC] + 1973-01-01T00:00:11Z[TAI]
;;                                    |
;;          1973-01-01T00:00:00Z[UTC] + 1973-01-01T00:00:12Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                   -- 94694399[UTC] + 94694410[TAI] --   delay 11
;;                    |               |               | 1
;;              leap  |               + 94694411[TAI] --   delay 11.5
;;                    |               |               | 2
;;                   -- 94694400[UTC] + 94694412[TAI] --   delay 12
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 3rd leap second
;;
;;Leap second UTC timestamp: 1973-12-31T23:59:60Z
;;After leap second UTC is behind TAI by: 13 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1973-12-31T23:59:59Z[UTC] + 1974-01-01T00:00:11Z[TAI]
;;                                    |
;;  leap    1973-12-31T23:59:60Z[UTC] + 1974-01-01T00:00:12Z[TAI]
;;                                    |
;;          1974-01-01T00:00:00Z[UTC] + 1974-01-01T00:00:13Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --   126230399[UTC] + 126230411[TAI]  --   delay 12
;;                 |                  |                 | 1
;;           leap  |                  + 126230412[TAI]  --   delay 12.5
;;                 |                  |                 | 2
;;                --   126230400[UTC] + 126230413[TAI]  --   delay 13
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 4th leap second
;;
;;Leap second UTC timestamp: 1974-12-31T23:59:60Z
;;After leap second UTC is behind TAI by: 14 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1974-12-31T23:59:59Z[UTC] + 1975-01-01T00:00:12Z[TAI]
;;                                    |
;;  leap    1974-12-31T23:59:60Z[UTC] + 1975-01-01T00:00:13Z[TAI]
;;                                    |
;;          1975-01-01T00:00:00Z[UTC] + 1975-01-01T00:00:14Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --   157766399[UTC] + 157766412[TAI]  --   delay 13
;;                 |                  |                 | 1
;;           leap  |                  + 157766413[TAI]  --   delay 13.5
;;                 |                  |                 | 2
;;                --   157766400[UTC] + 157766414[TAI]  --   delay 14
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 5th leap second
;;
;;Leap second UTC timestamp: 1975-12-31T23:59:60Z
;;After leap second UTC is behind TAI by: 15 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1975-12-31T23:59:59Z[UTC] + 1976-01-01T00:00:13Z[TAI]
;;                                    |
;;  leap    1975-12-31T23:59:60Z[UTC] + 1976-01-01T00:00:14Z[TAI]
;;                                    |
;;          1976-01-01T00:00:00Z[UTC] + 1976-01-01T00:00:15Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --   189302399[UTC] + 189302413[TAI]  --   delay 14
;;                 |                  |                 | 1
;;           leap  |                  + 189302414[TAI]  --   delay 14.5
;;                 |                  |                 | 2
;;                --   189302400[UTC] + 189302415[TAI]  --   delay 15
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 6th leap second
;;
;;Leap second UTC timestamp: 1976-12-31T23:59:60Z
;;After leap second UTC is behind TAI by: 16 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1976-12-31T23:59:59Z[UTC] + 1977-01-01T00:00:14Z[TAI]
;;                                    |
;;  leap    1976-12-31T23:59:60Z[UTC] + 1977-01-01T00:00:15Z[TAI]
;;                                    |
;;          1977-01-01T00:00:00Z[UTC] + 1977-01-01T00:00:16Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --   220924799[UTC] + 220924814[TAI]  --   delay 15
;;                 |                  |                 | 1
;;           leap  |                  + 220924815[TAI]  --   delay 15.5
;;                 |                  |                 | 2
;;                --   220924800[UTC] + 220924816[TAI]  --   delay 16
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 7th leap second
;;
;;Leap second UTC timestamp: 1977-12-31T23:59:60Z
;;After leap second UTC is behind TAI by: 17 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1977-12-31T23:59:59Z[UTC] + 1978-01-01T00:00:15Z[TAI]
;;                                    |
;;  leap    1977-12-31T23:59:60Z[UTC] + 1978-01-01T00:00:16Z[TAI]
;;                                    |
;;          1978-01-01T00:00:00Z[UTC] + 1978-01-01T00:00:17Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --   252460799[UTC] + 252460815[TAI]  --   delay 16
;;                 |                  |                 | 1
;;           leap  |                  + 252460816[TAI]  --   delay 16.5
;;                 |                  |                 | 2
;;                --   252460800[UTC] + 252460817[TAI]  --   delay 17
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 8th leap second
;;
;;Leap second UTC timestamp: 1978-12-31T23:59:60Z
;;After leap second UTC is behind TAI by: 18 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1978-12-31T23:59:59Z[UTC] + 1979-01-01T00:00:16Z[TAI]
;;                                    |
;;  leap    1978-12-31T23:59:60Z[UTC] + 1979-01-01T00:00:17Z[TAI]
;;                                    |
;;          1979-01-01T00:00:00Z[UTC] + 1979-01-01T00:00:18Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --   283996799[UTC] + 283996816[TAI]  --   delay 17
;;                 |                  |                 | 1
;;           leap  |                  + 283996817[TAI]  --   delay 17.5
;;                 |                  |                 | 2
;;                --   283996800[UTC] + 283996818[TAI]  --   delay 18
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 9th leap second
;;
;;Leap second UTC timestamp: 1979-12-31T23:59:60Z
;;After leap second UTC is behind TAI by: 19 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1979-12-31T23:59:59Z[UTC] + 1980-01-01T00:00:17Z[TAI]
;;                                    |
;;  leap    1979-12-31T23:59:60Z[UTC] + 1980-01-01T00:00:18Z[TAI]
;;                                    |
;;          1980-01-01T00:00:00Z[UTC] + 1980-01-01T00:00:19Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --   315532799[UTC] + 315532817[TAI]  --   delay 18
;;                 |                  |                 | 1
;;           leap  |                  + 315532818[TAI]  --   delay 18.5
;;                 |                  |                 | 2
;;                --   315532800[UTC] + 315532819[TAI]  --   delay 19
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 10th leap second
;;
;;Leap second UTC timestamp: 1981-06-30T23:59:60Z
;;After leap second UTC is behind TAI by: 20 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1981-06-30T23:59:59Z[UTC] + 1981-07-01T00:00:18Z[TAI]
;;                                    |
;;  leap    1981-06-30T23:59:60Z[UTC] + 1981-07-01T00:00:19Z[TAI]
;;                                    |
;;          1981-07-01T00:00:00Z[UTC] + 1981-07-01T00:00:20Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --   362793599[UTC] + 362793618[TAI]  --   delay 19
;;                 |                  |                 | 1
;;           leap  |                  + 362793619[TAI]  --   delay 19.5
;;                 |                  |                 | 2
;;                --   362793600[UTC] + 362793620[TAI]  --   delay 20
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 11th leap second
;;
;;Leap second UTC timestamp: 1982-06-30T23:59:60Z
;;After leap second UTC is behind TAI by: 21 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1982-06-30T23:59:59Z[UTC] + 1982-07-01T00:00:19Z[TAI]
;;                                    |
;;  leap    1982-06-30T23:59:60Z[UTC] + 1982-07-01T00:00:20Z[TAI]
;;                                    |
;;          1982-07-01T00:00:00Z[UTC] + 1982-07-01T00:00:21Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --   394329599[UTC] + 394329619[TAI]  --   delay 20
;;                 |                  |                 | 1
;;           leap  |                  + 394329620[TAI]  --   delay 20.5
;;                 |                  |                 | 2
;;                --   394329600[UTC] + 394329621[TAI]  --   delay 21
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 12th leap second
;;
;;Leap second UTC timestamp: 1983-06-30T23:59:60Z
;;After leap second UTC is behind TAI by: 22 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1983-06-30T23:59:59Z[UTC] + 1983-07-01T00:00:20Z[TAI]
;;                                    |
;;  leap    1983-06-30T23:59:60Z[UTC] + 1983-07-01T00:00:21Z[TAI]
;;                                    |
;;          1983-07-01T00:00:00Z[UTC] + 1983-07-01T00:00:22Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --   425865599[UTC] + 425865620[TAI]  --   delay 21
;;                 |                  |                 | 1
;;           leap  |                  + 425865621[TAI]  --   delay 21.5
;;                 |                  |                 | 2
;;                --   425865600[UTC] + 425865622[TAI]  --   delay 22
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 13th leap second
;;
;;Leap second UTC timestamp: 1985-06-30T23:59:60Z
;;After leap second UTC is behind TAI by: 23 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1985-06-30T23:59:59Z[UTC] + 1985-07-01T00:00:21Z[TAI]
;;                                    |
;;  leap    1985-06-30T23:59:60Z[UTC] + 1985-07-01T00:00:22Z[TAI]
;;                                    |
;;          1985-07-01T00:00:00Z[UTC] + 1985-07-01T00:00:23Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --   489023999[UTC] + 489024021[TAI]  --   delay 22
;;                 |                  |                 | 1
;;           leap  |                  + 489024022[TAI]  --   delay 22.5
;;                 |                  |                 | 2
;;                --   489024000[UTC] + 489024023[TAI]  --   delay 23
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 14th leap second
;;
;;Leap second UTC timestamp: 1987-12-31T23:59:60Z
;;After leap second UTC is behind TAI by: 24 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1987-12-31T23:59:59Z[UTC] + 1988-01-01T00:00:22Z[TAI]
;;                                    |
;;  leap    1987-12-31T23:59:60Z[UTC] + 1988-01-01T00:00:23Z[TAI]
;;                                    |
;;          1988-01-01T00:00:00Z[UTC] + 1988-01-01T00:00:24Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --   567993599[UTC] + 567993622[TAI]  --   delay 23
;;                 |                  |                 | 1
;;           leap  |                  + 567993623[TAI]  --   delay 23.5
;;                 |                  |                 | 2
;;                --   567993600[UTC] + 567993624[TAI]  --   delay 24
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 15th leap second
;;
;;Leap second UTC timestamp: 1989-12-31T23:59:60Z
;;After leap second UTC is behind TAI by: 25 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1989-12-31T23:59:59Z[UTC] + 1990-01-01T00:00:23Z[TAI]
;;                                    |
;;  leap    1989-12-31T23:59:60Z[UTC] + 1990-01-01T00:00:24Z[TAI]
;;                                    |
;;          1990-01-01T00:00:00Z[UTC] + 1990-01-01T00:00:25Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --   631151999[UTC] + 631152025[TAI]  --   delay 24
;;                 |                  |                 | 1
;;           leap  |                  + 631152025[TAI]  --   delay 24.5
;;                 |                  |                 | 2
;;                --   631152000[UTC] + 631152025[TAI]  --   delay 25
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 16th leap second
;;
;;Leap second UTC timestamp: 1990-12-31T23:59:60Z
;;After leap second UTC is behind TAI by: 26 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1990-12-31T23:59:59Z[UTC] + 1991-01-01T00:00:24Z[TAI]
;;                                    |
;;  leap    1990-12-31T23:59:60Z[UTC] + 1991-01-01T00:00:25Z[TAI]
;;                                    |
;;          1991-01-01T00:00:00Z[UTC] + 1991-01-01T00:00:26Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --   662687999[UTC] + 662688024[TAI]  --   delay 25
;;                 |                  |                 | 1
;;           leap  |                  + 662688025[TAI]  --   delay 25.5
;;                 |                  |                 | 2
;;                --   662688000[UTC] + 662688026[TAI]  --   delay 26
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 17th leap second
;;
;;Leap second UTC timestamp: 1992-06-30T23:59:60Z
;;After leap second UTC is behind TAI by: 27 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1992-06-30T23:59:59Z[UTC] + 1992-07-01T00:00:25Z[TAI]
;;                                    |
;;  leap    1992-06-30T23:59:60Z[UTC] + 1992-07-01T00:00:26Z[TAI]
;;                                    |
;;          1992-07-01T00:00:00Z[UTC] + 1992-07-01T00:00:27Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --   709948799[UTC] + 709948825[TAI]  --   delay 26
;;                 |                  |                 | 1
;;           leap  |                  + 709948826[TAI]  --   delay 26.5
;;                 |                  |                 | 2
;;                --   709948800[UTC] + 709948827[TAI]  --   delay 27
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 18th leap second
;;
;;Leap second UTC timestamp: 1993-06-30T23:59:60Z
;;After leap second UTC is behind TAI by: 28 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1993-06-30T23:59:59Z[UTC] + 1993-07-01T00:00:26Z[TAI]
;;                                    |
;;  leap    1993-06-30T23:59:60Z[UTC] + 1993-07-01T00:00:27Z[TAI]
;;                                    |
;;          1993-07-01T00:00:00Z[UTC] + 1993-07-01T00:00:28Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --   741484799[UTC] + 741484826[TAI]  --   delay 27
;;                 |                  |                 | 1
;;           leap  |                  + 741484827[TAI]  --   delay 27.5
;;                 |                  |                 | 2
;;                --   741484800[UTC] + 741484828[TAI]  --   delay 28
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 19th leap second
;;
;;Leap second UTC timestamp: 1994-06-30T23:59:60Z
;;After leap second UTC is behind TAI by: 29 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1994-06-30T23:59:59Z[UTC] + 1994-07-01T00:00:27Z[TAI]
;;                                    |
;;  leap    1994-06-30T23:59:60Z[UTC] + 1994-07-01T00:00:28Z[TAI]
;;                                    |
;;          1994-07-01T00:00:00Z[UTC] + 1994-07-01T00:00:29Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --   773020799[UTC] + 773020827[TAI]  --   delay 28
;;                 |                  |                 | 1
;;           leap  |                  + 773020828[TAI]  --   delay 28.5
;;                 |                  |                 | 2
;;                --   773020800[UTC] + 773020829[TAI]  --   delay 29
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 20th leap second
;;
;;Leap second UTC timestamp: 1995-12-31T23:59:60Z
;;After leap second UTC is behind TAI by: 30 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1995-12-31T23:59:59Z[UTC] + 1996-01-01T00:00:28Z[TAI]
;;                                    |
;;  leap    1995-12-31T23:59:60Z[UTC] + 1996-01-01T00:00:29Z[TAI]
;;                                    |
;;          1996-01-01T00:00:00Z[UTC] + 1996-01-01T00:00:30Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --   820454399[UTC] + 820454428[TAI]  --   delay 29
;;                 |                  |                 | 1
;;           leap  |                  + 820454429[TAI]  --   delay 29.5
;;                 |                  |                 | 2
;;                --   820454400[UTC] + 820454430[TAI]  --   delay 30
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 21st leap second
;;
;;Leap second UTC timestamp: 1997-06-30T23:59:60Z
;;After leap second UTC is behind TAI by: 31 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1997-06-30T23:59:59Z[UTC] + 1997-07-01T00:00:29Z[TAI]
;;                                    |
;;  leap    1997-06-30T23:59:60Z[UTC] + 1997-07-01T00:00:30Z[TAI]
;;                                    |
;;          1997-07-01T00:00:00Z[UTC] + 1997-07-01T00:00:31Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --   867715199[UTC] + 867715229[TAI]  --   delay 30
;;                 |                  |                 | 1
;;           leap  |                  + 867715230[TAI]  --   delay 30.5
;;                 |                  |                 | 2
;;                --   867715200[UTC] + 867715231[TAI]  --   delay 31
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 22nd leap second
;;
;;Leap second UTC timestamp: 1998-12-31T23:59:60Z
;;After leap second UTC is behind TAI by: 32 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          1998-12-31T23:59:59Z[UTC] + 1999-01-01T00:00:30Z[TAI]
;;                                    |
;;  leap    1998-12-31T23:59:60Z[UTC] + 1999-01-01T00:00:31Z[TAI]
;;                                    |
;;          1999-01-01T00:00:00Z[UTC] + 1999-01-01T00:00:32Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --   915148799[UTC] + 915148830[TAI]  --   delay 31
;;                 |                  |                 | 1
;;           leap  |                  + 915148831[TAI]  --   delay 31.5
;;                 |                  |                 | 2
;;                --   915148800[UTC] + 915148832[TAI]  --   delay 32
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 23rd leap second
;;
;;Leap second UTC timestamp: 2005-12-31T23:59:60Z
;;After leap second UTC is behind TAI by: 33 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          2005-12-31T23:59:59Z[UTC] + 2006-01-01T00:00:31Z[TAI]
;;                                    |
;;  leap    2005-12-31T23:59:60Z[UTC] + 2006-01-01T00:00:32Z[TAI]
;;                                    |
;;          2006-01-01T00:00:00Z[UTC] + 2006-01-01T00:00:33Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --  1136073599[UTC] + 1136073631[TAI]  --   delay 32
;;                 |                  |                 | 1
;;           leap  |                  + 1136073632[TAI]  --   delay 32.5
;;                 |                  |                 | 2
;;                --  1136073600[UTC] + 1136073633[TAI]  --   delay 33
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 24th leap second
;;
;;Leap second UTC timestamp: 2008-12-31T23:59:60Z
;;After leap second UTC is behind TAI by: 34 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          2008-12-31T23:59:59Z[UTC] + 2009-01-01T00:00:32Z[TAI]
;;                                    |
;;  leap    2008-12-31T23:59:60Z[UTC] + 2009-01-01T00:00:33Z[TAI]
;;                                    |
;;          2009-01-01T00:00:00Z[UTC] + 2009-01-01T00:00:34Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --  1230767999[UTC] + 1230768032[TAI]  --   delay 33
;;                 |                  |                 | 1
;;           leap  |                  + 1230768033[TAI]  --   delay 33.5
;;                 |                  |                 | 2
;;                --  1230768000[UTC] + 1230768034[TAI]  --   delay 34
;;                                    |
;;                                 future

;;; --------------------------------------------------------------------
;;; 25th leap second
;;
;;Leap second UTC timestamp: 2012-06-30T23:59:60Z
;;After leap second UTC is behind TAI by: 35 seconds
;;
;;UTC ISO 8601 timestamps:          past
;;                                    |
;;          2012-12-31T23:59:59Z[UTC] + 2013-01-01T00:00:33Z[TAI]
;;                                    |
;;  leap    2012-12-31T23:59:60Z[UTC] + 2013-01-01T00:00:34Z[TAI]
;;                                    |
;;          2013-01-01T00:00:00Z[UTC] + 2013-01-01T00:00:35Z[TAI]
;;                                    |
;;                                 future
;;
;;Epoch timestamps:                 past
;;                                    |
;;                --  1341100799[UTC] + 1341100833[TAI]  --   delay 34
;;                 |                  |                 | 1
;;           leap  |                  + 1341100834[TAI]  --   delay 34.5
;;                 |                  |                 | 2
;;                --  1341100800[UTC] + 1341100835[TAI]  --   delay 35
;;                                    |
;;                                 future


;;;; Julian Date
;;
;;The Julian Date (JD, <http://en.wikipedia.org/wiki/Julian_day>) is the
;;interval   of  time   in   days   and  fractions   of   a  day   since
;;-4714-11-24T12:00:00Z  (November 24,  -4714 at  noon, UTC  scale, time
;;zone zero).
;;
;;The Julian Day  Number (JDN) is the integral part  of the Julian Date.
;;The day  commencing at  the above-mentioned  epoch is  zero.  Negative
;;values  can be  used  for  preceding dates,  though  they predate  all
;;recorded history.
;;
;;The Modified  Julian Day Number (MJDN)  represents a point in  time as
;;number  of  days  since  1858-11-17T00:00:00Z (November  17,  1858  at
;;midnight, UTC scale, time zone zero).
;;
;;The MDJN is 4800001/2 = 2400000.5 days less than the JDN:
;;
;;   JDN - MJDN = 4800001/2
;;
;;this brings the numbers into a more manageable numeric range and makes
;;the day numbers change at midnight UTC rather than noon.
;;
;;Julian Date  test values can be  computed with the calculator  at (URL
;;last verified Thu Jul 29, 2010):
;;
;;   <http://www.imcce.fr/en/grandpublic/temps/jour_julien.php>
;;   <http://www.imcce.fr/langues/en/grandpublic/temps/jour_julien.php>
;;
;;the number  computed by  that calculator is  the JD  at year/month/day
;;hour:minute:second.


#!r6rs
(library (srfi :19 time core)
  (export

    time-duration		time-monotonic		time-process
    time-tai			time-thread		time-utc

    current-date		current-julian-day	current-modified-julian-day
    current-time		time-resolution

    (rename (true-make-time	make-time))
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

    (rename (true-make-date	make-date))
    date?
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

    date->string			string->date

;;; --------------------------------------------------------------------

    ;; extension constants
    NUMBER-OF-NANOSECONDS-IN-A-SECOND
    NUMBER-OF-SECONDS-IN-A-DAY
    NUMBER-OF-SECONDS-IN-HALF-A-DAY
    TAI-EPOCH-IN-JULIAN-DAYS
    NUMBER-OF-TAI-NANOSECONDS-IN-BIZARRE-UTC-SECOND
    UTC-SECONDS-AT-BEG-OF-BIZARRE-SECOND
    UTC-SECONDS-AT-END-OF-BIZARRE-SECOND
    TAI-SECONDS-AT-BEG-OF-BIZARRE-SECOND
    TAI-SECONDS-AT-END-OF-BIZARRE-SECOND

;;; --------------------------------------------------------------------

    ;; extension functions
    LEAP-SECONDS-TABLE
    utc-seconds-in-leap-second?		tai-seconds-in-leap-second?
    leap-seconds-table.utc-leap-second-end
    leap-seconds-table.utc-to-tai-increment-after-leap-second

    quasi-time=?)
  (import (except (vicare)
		  current-time
		  time-nanosecond
		  time-second
		  time-gmt-offset
		  time
		  time?)
    (prefix (only (vicare)
		  current-time
		  time-nanosecond
		  time-second
		  time-gmt-offset)
	    host.)
    (prefix (vicare unsafe-operations)
	    $)
    (vicare syntactic-extensions)
    (vicare arguments validation)
    (srfi :6 basic-string-ports))


;;;; constants

(define-constant NUMBER-OF-NANOSECONDS-IN-A-SECOND	#e1e9)
(define-constant NUMBER-OF-SECONDS-IN-A-DAY		86400)
(define-constant NUMBER-OF-SECONDS-IN-HALF-A-DAY	43200)

(define-constant TAI-EPOCH-IN-JULIAN-DAYS		4881175/2)

;;Diference between Julian Day Number and Modified Julian Day Number.
;;
;; JDN - MJDN = 4800001/2
;; JDN  = MJDN + 4800001/2
;; MJDN = JDN  - 4800001/2
;;
(define-constant JDN-MJDN 4800001/2)

;;; --------------------------------------------------------------------

(define-constant NUMBER-OF-TAI-NANOSECONDS-IN-BIZARRE-UTC-SECOND
  (* 11 NUMBER-OF-NANOSECONDS-IN-A-SECOND))

(define-constant UTC-SECONDS-AT-BEG-OF-BIZARRE-SECOND	(- 63072000 1))
(define-constant UTC-SECONDS-AT-END-OF-BIZARRE-SECOND	63072000)
(define-constant TAI-SECONDS-AT-BEG-OF-BIZARRE-SECOND	(- 63072000 1))
(define-constant TAI-SECONDS-AT-END-OF-BIZARRE-SECOND	(+ 63072000 10))

;;; --------------------------------------------------------------------
;;; symbols required by the SRFI specification

(let-syntax ((define-time-type (syntax-rules ()
				 ((_ ?type)
				  (define ?type (quote ?type))))))
  (define-time-type time-tai)
  (define-time-type time-utc)
  (define-time-type time-monotonic)
  (define-time-type time-thread)
  (define-time-type time-process)
  (define-time-type time-duration))

;;; --------------------------------------------------------------------
;;; locale dependent constants

(define LOCALE-NUMBER-SEPARATOR
  ".")

(define LOCALE-ABBR-WEEKDAY-VECTOR
  #'("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

(define LOCALE-LONG-WEEKDAY-VECTOR
  '#("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

(define LOCALE-ABBR-MONTH-VECTOR
  '#(""		;note empty string in 0th place
     "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define LOCALE-LONG-MONTH-VECTOR
  '#(""		;note empty string in 0th place
     "January"		"February"	"March"
     "April"		"May"		"June"
     "July"		"August"	"September"
     "October"		"November"	"December"))

(define LOCALE-PM "PM")
(define LOCALE-AM "AM")

;;See DATE->STRING for the meaning of these.
(define LOCALE-DATE-TIME-FORMAT		"~a ~b ~d ~H:~M:~S~z ~Y")
(define LOCALE-SHORT-DATE-FORMAT	"~m/~d/~y")
(define LOCALE-TIME-FORMAT		"~H:~M:~S")
(define ISO-8601-DATE-TIME-FORMAT	"~Y-~m-~dT~H:~M:~S~z")


;;;; common arguments validation

(define-argument-validation (seconds who obj)
  (or (fixnum? obj)
      (bignum? obj))
  (assertion-violation who
    "expected exact integer as number of seconds argument" obj))

(define-argument-validation (nanoseconds who obj)
  (or (fixnum? obj)
      (bignum? obj))
  (assertion-violation who
    "expected exact integer as number of nanoseconds argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (date-seconds who year month day hour minute second)
  (and (fixnum? second)
       ($fx>= second 0)
       (or ($fx<= second 58)
	   ($fx<= second 60)))
  (assertion-violation who
    "expected exact integer in range [0, 59] or [0, 60] as number of seconds argument"
    year month day hour minute second))

(define-argument-validation (date-nanoseconds who obj)
  (and (or (fixnum? obj)
	   (bignum? obj))
       (>= obj 0)
       (<= obj 999999999))
  (assertion-violation who
    "expected exact integer in range [0, 999999999] as number of nanoseconds argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (julian-day-number who obj)
  (or (fixnum? obj)
      (bignum? obj))
  (assertion-violation who "expected julian day number as argument" obj))

(define-argument-validation (modified-julian-day-number who obj)
  (or (fixnum? obj)
      (bignum? obj))
  (assertion-violation who "expected modified julian day number as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (time-zone-offset who obj)
  (fixnum? obj)
  (assertion-violation who "expected time zone offset as argument" obj))


;;;; implementation specific stuff

(let-syntax
    ((define-not-implemented (syntax-rules ()
			       ((_ ?who)
				(define (?who . args)
				  (assertion-violation '?who "not implemented"))))))
  (define-not-implemented host.cumulative-thread-time)
  (define-not-implemented host.cumulative-process-time)
  (define-not-implemented host.cumulative-gc-time))

;;Vicare uses gettimeofday() which gives microseconds, so our resolution
;;is 1000 nanoseconds.
(define host.time-resolution 1000)

(define host.timezone-offset
  (host.time-gmt-offset (host.current-time)))

(define time-resolution
  (case-lambda
   (()
    host.time-resolution)
   ((clock-type)
    host.time-resolution)))

(define (%local-tz-offset)
  host.timezone-offset)


;;;; leap seconds handling
;;
;;Each entry the following table is:
;;
;;   (<UTC SECONDS SINCE EPOCH> . <NUMBER OF SECONDS TO ADD FOR TAI>)
;;
;;note they go higher to lower, and end in 1972:
;;
;;* The  <UTC SECONDS  SINCE EPOCH>  is the count  of seconds  since the
;;  Epoch,  on the  UTC  scale, of  the seconds  right  after each  leap
;;  second.
;;
;;* The <NUMBER OF  SECONDS TO ADD FOR TAI> is the  number of seconds to
;;  add to the  UTC count of seconds  since the Epoch to  obtain the TAI
;;  count of seconds  since the Epoch, in the interval  between two leap
;;  seconds.
;;
;;*NOTE* This table  was last updated on  Jan 18, 2013.  It  may be that
;;this  is the  last update  needed, because  there is  discussion about
;;stopping the  definition of leap  seconds.  (Marco Maggi; Sat  Jan 19,
;;2013)
;;
;;Whenever a new leap second is  introduced: to update the table use the
;;script "read-table.sps" in this directory.
;;
(define-constant LEAP-SECONDS-TABLE
  '((1341100800 . 35)
    (1230768000 . 34)
    (1136073600 . 33)
    (915148800 . 32)
    (867715200 . 31)
    (820454400 . 30)
    (773020800 . 29)
    (741484800 . 28)
    (709948800 . 27)
    (662688000 . 26)
    (631152000 . 25)
    (567993600 . 24)
    (489024000 . 23)
    (425865600 . 22)
    (394329600 . 21)
    (362793600 . 20)
    (315532800 . 19)
    (283996800 . 18)
    (252460800 . 17)
    (220924800 . 16)
    (189302400 . 15)
    (157766400 . 14)
    (126230400 . 13)
    (94694400 . 12)
    (78796800 . 11)
    (63072000 . 10)))

(define leap-seconds-table.utc-leap-second-end
  car)

(define leap-seconds-table.utc-to-tai-increment-after-leap-second
  cdr)

;;; --------------------------------------------------------------------

(define (%utc->tai:leap-second-delta utc-seconds)
  ;;Going from TAI seconds to UTC seconds ...
  (letrec ((lsd (lambda (table)
		  (cond ((>= utc-seconds (caar table))
			 (cdar table))
			(else
			 (lsd (cdr table)))))))
    (if (< utc-seconds UTC-SECONDS-AT-END-OF-BIZARRE-SECOND)
	0
      (lsd LEAP-SECONDS-TABLE))))

(define (%tai->utc:leap-second-delta tai-seconds)
  ;;Going from TAI seconds to UTC seconds ...
  (letrec ((lsd (lambda (table)
		  (cond ((null? table)
			 0)
			((<= (cdar table) (- tai-seconds (caar table)))
                         (cdar table))
			(else
			 (lsd (cdr table)))))))
    (if (< tai-seconds UTC-SECONDS-AT-END-OF-BIZARRE-SECOND)
	0
      (lsd LEAP-SECONDS-TABLE))))

;;; --------------------------------------------------------------------

(define (%tai-seconds-in-utc-bizarre-second? tai-seconds)
  ;;Return true  if the  given TAI  time seconds  since the  Epoch falls
  ;;inside the bizarre UTC second.
  ;;
  (and (>= tai-seconds 63072000)
       (<  tai-seconds 63072010)))

(define (%tai-time->bizarre-utc-time tai-nanoseconds tai-seconds)
  ;;Given a  TAI time  since the  Epoch falling  inside the  bizarre UTC
  ;;second: compute the corresponding UTC  time since the Epoch.  Return
  ;;two values:  (1) the count of  UTC nanoseconds and (2)  the count of
  ;;UTC seconds as exact integers.
  ;;
  (define tai-nanoseconds-since-bizarre-beginning
    (+ tai-nanoseconds (* NUMBER-OF-NANOSECONDS-IN-A-SECOND
			  (- tai-seconds TAI-SECONDS-AT-BEG-OF-BIZARRE-SECOND))))
  (define fraction
    (/ tai-nanoseconds-since-bizarre-beginning
       NUMBER-OF-TAI-NANOSECONDS-IN-BIZARRE-UTC-SECOND))
  (define utc-nanoseconds-since-bizarre-beginning
    (* NUMBER-OF-NANOSECONDS-IN-A-SECOND fraction))
  (values (exact (round utc-nanoseconds-since-bizarre-beginning))
	  UTC-SECONDS-AT-BEG-OF-BIZARRE-SECOND))

;;; --------------------------------------------------------------------

(define (%utc-seconds-in-utc-bizarre-second? utc-seconds)
  (= utc-seconds (- 63072000 1)))

(define (%bizarre-utc-time->tai-time utc-nanoseconds utc-seconds)
  ;;Given a  UTC time  since the  Epoch falling  inside the  bizarre UTC
  ;;second: compute the corresponding TAI  time since the Epoch.  Return
  ;;two values:  (1) the count of  TAI nanoseconds and (2)  the count of
  ;;TAI seconds as exact integers.
  ;;
  ;;We could do:
  ;;
  ;;   (define TAI-NANOSECONDS-IN-BIZARRE-UTC-SECOND
  ;;     (* 11 NUMBER-OF-NANOSECONDS-IN-A-SECOND))
  ;;
  ;;   (define fraction
  ;;     (/ utc-nanoseconds NUMBER-OF-NANOSECONDS-IN-A-SECOND))
  ;;
  ;;   (define tai-nanoseconds-since-bizarre-beginning
  ;;     (* fraction TAI-NANOSECONDS-IN-BIZARRE-UTC-SECOND))
  ;;
  ;;substituting:
  ;;
  ;;   (define tai-nanoseconds-since-bizarre-beginning
  ;;     (* (/ utc-nanoseconds NUMBER-OF-NANOSECONDS-IN-A-SECOND)
  ;;        (* 11 NUMBER-OF-NANOSECONDS-IN-A-SECOND)))
  ;;
  ;;and it is obviously simplified into:
  ;;
  ;;    (define tai-nanoseconds-since-bizarre-beginning
  ;;      (* 11 utc-nanoseconds))
  ;;
  (receive (nanos secs)
      ($nanoseconds->nanos+secs (* utc-nanoseconds 11))
    (values nanos (+ utc-seconds secs))))

;;; --------------------------------------------------------------------

(define (utc-seconds-in-leap-second? utc-seconds)
  (cond ((assv (add1 utc-seconds) LEAP-SECONDS-TABLE)
	 => (lambda (pair)
	      (not (= (car pair) UTC-SECONDS-AT-END-OF-BIZARRE-SECOND))))
	(else #f)))

(define (tai-seconds-in-leap-second? tai-seconds)
  (let next-entry ((table LEAP-SECONDS-TABLE))
    (if (null? table)
	#f
      (let ((end-of-leap ($caar LEAP-SECONDS-TABLE))
	    (tai-utc     ($cdar LEAP-SECONDS-TABLE)))
	(let ((past (+ end-of-leap tai-utc)))
	  (or (and (>= tai-seconds (+ -2 past))
		   (<  tai-seconds past))
	      (next-entry ($cdr LEAP-SECONDS-TABLE))))))))


;;;; time structure

(define-struct time
  (type nanosecond second))

(define (true-make-time type nanoseconds seconds)
  (define who 'make-time)
  (with-arguments-validation (who)
      ((time-type	type)
       (real		nanoseconds)
       (real		seconds))
    (let ((nanoseconds	(exact (round nanoseconds)))
	  (seconds	(exact (round seconds))))
      (with-arguments-validation (who)
	  ((seconds	nanoseconds)
	   (nanoseconds	seconds))
	(receive (nanos secs)
	    ($nanoseconds->nanos+secs nanoseconds)
	  (make-time type nanos (+ seconds secs)))))))

(define (copy-time time)
  (define who 'copy-time)
  (with-arguments-validation (who)
      ((time	time))
    ($copy-time time)))

(define ($copy-time time)
  (make-time ($time-type	time)
	     ($time-nanosecond	time)
	     ($time-second	time)))

;;; --------------------------------------------------------------------

(define-argument-validation (time-type who obj)
  (memq obj '(time-tai
	      time-utc
	      time-monotonic
	      time-thread
	      time-process
	      time-duration))
  (assertion-violation who "expected time object as argument" obj))

(define-argument-validation (time who obj)
  (time? obj)
  (assertion-violation who "expected time object as argument" obj))

(define-argument-validation (same-time-type who time1 time2)
  (eq? ($time-type time1)
       ($time-type time2))
  (assertion-violation who "expected time objects with the same type" time1 time2))

(let-syntax
    ((define-time-type-validation
       (syntax-rules ()
	 ((_ ?type)
	  (define-argument-validation (?type who obj)
	    (and (time? obj)
		 (eq? '?type ($time-type obj)))
	    (assertion-violation who
	      (string-append "invalid time type, expected " (symbol->string '?type))
	      obj))))))
  (define-time-type-validation time-tai)
  (define-time-type-validation time-utc)
  (define-time-type-validation time-monotonic)
  (define-time-type-validation time-thread)
  (define-time-type-validation time-process)
  (define-time-type-validation time-duration))


;;;; current time

(module (current-time)

  (define current-time
    (case-lambda
     (()
      (current-time time-utc))
     ((clock-type)
      (case-symbols clock-type
	((time-tai)		(%current-time-tai))
	((time-utc)		(%current-time-utc))
	((time-monotonic)	(%current-time-monotonic))
	((time-thread)		(%current-time-thread))
	((time-process)		(%current-time-process))
	(else
	 (error 'current-time "invalid clock type" clock-type))))))

  (define (%current-time-utc)
    (%make-time-helper host.current-time time-utc values))

  (define (%current-time-tai)
    (%make-time-helper host.current-time time-tai my:leap-second-helper))

  (define (%current-time-ms-time time-type proc)
    (let ((current-ms (proc)))
      (make-time time-type
		 (* (remainder current-ms 1000) 1000)
		 (quotient current-ms 1000))))

  (define (%current-time-monotonic)
    (%make-time-helper host.current-time time-monotonic my:leap-second-helper))

  (define (%current-time-thread)
    (%make-time-helper host.cumulative-thread-time time-thread values))

  (define (%current-time-process)
    (%make-time-helper host.cumulative-process-time time-process values))

;;; --------------------------------------------------------------------

  (define (%make-time-helper current-time type proc)
    (let ((x (current-time)))
      (make-time type
		 (host.time-nanosecond x)
		 (proc (host.time-second x)))))

  (define (my:leap-second-helper s)
    (+ s (%utc->tai:leap-second-delta s)))

  #| end of module |# )


;;;; time comparisons

(define (time=? time1 time2)
  (define who 'time=?)
  (with-arguments-validation (who)
      ((time		time1)
       (time		time2)
       (same-time-type	time1 time2))
    ($time=? time1 time2)))

(define ($time=? time1 time2)
  (and (= ($time-second time1)
	  ($time-second time2))
       (= ($time-nanosecond time1)
	  ($time-nanosecond time2))))

;;; --------------------------------------------------------------------

(define (time>? time1 time2)
  (define who 'time>?)
  (with-arguments-validation (who)
      ((time		time1)
       (time		time2)
       (same-time-type	time1 time2))
    ($time>? time1 time2)))

(define ($time>? time1 time2)
  (or (> ($time-second time1)
	 ($time-second time2))
      (and (= ($time-second time1)
	      ($time-second time2))
	   (> ($time-nanosecond time1)
	      ($time-nanosecond time2)))))

;;; --------------------------------------------------------------------

(define (time<? time1 time2)
  (define who 'time<?)
  (with-arguments-validation (who)
      ((time		time1)
       (time		time2)
       (same-time-type	time1 time2))
    ($time<? time1 time2)))

(define ($time<? time1 time2)
  (or (< ($time-second time1)
	 ($time-second time2))
      (and (= ($time-second time1)
	      ($time-second time2))
	   (< ($time-nanosecond time1)
	      ($time-nanosecond time2)))))

;;; --------------------------------------------------------------------

(define (time>=? time1 time2)
  (define who 'time>=?)
  (with-arguments-validation (who)
      ((time		time1)
       (time		time2)
       (same-time-type	time1 time2))
    ($time>=? time1 time2)))

(define ($time>=? time1 time2)
  (or (> ($time-second time1)
	 ($time-second time2))
      (and (= ($time-second time1)
	      ($time-second time2))
	   (>= ($time-nanosecond time1)
	       ($time-nanosecond time2)))))

;;; --------------------------------------------------------------------

(define (time<=? time1 time2)
  (define who 'time<=?)
  (with-arguments-validation (who)
      ((time		time1)
       (time		time2)
       (same-time-type	time1 time2))
    ($time<=? time1 time2)))

(define ($time<=? time1 time2)
  (or (< ($time-second time1)
	 ($time-second time2))
      (and (= ($time-second time1)
	      ($time-second time2))
	   (<= ($time-nanosecond time1)
	       ($time-nanosecond time2)))))

;;; --------------------------------------------------------------------
;;; time comparison extended functions

(define (quasi-time=? time1 time2 nanoseconds-tolerance)
  (define who 'quasi-time=?)
  (with-arguments-validation (who)
      ((time		time1)
       (time		time2)
       (same-time-type	time1 time2)
       (exact-integer	nanoseconds-tolerance))
    ($quasi-time=? time1 time2 nanoseconds-tolerance)))

(define ($quasi-time=? time1 time2 nanoseconds-tolerance)
  (< (abs (- (+ ($time-nanosecond time1)
		(* NUMBER-OF-NANOSECONDS-IN-A-SECOND ($time-second time1)))
	     (+ ($time-nanosecond time2)
		(* NUMBER-OF-NANOSECONDS-IN-A-SECOND ($time-second time2)))))
     nanoseconds-tolerance))


;;;; time arithmetic

(module ($nanoseconds->nanos+secs
	 time-difference		$time-difference
	 time-difference!		$time-difference!
	 add-duration			$add-duration
	 add-duration!			$add-duration!
	 subtract-duration		$subtract-duration
	 subtract-duration!		$subtract-duration!)

  (define (time-difference time1 time2)
    (define who 'time-difference)
    (with-arguments-validation (who)
	((time			time1)
	 (time			time2)
	 (same-time-type	time1 time2))
      ($time-difference time1 time2)))

  (define (time-difference! time1 time2)
    (define who 'time-difference!)
    (with-arguments-validation (who)
	((time			time1)
	 (time			time2)
	 (same-time-type	time1 time2))
      ($time-difference! time1 time1 time2)))

  (define (add-duration time duration)
    (define who 'add-duration)
    (with-arguments-validation (who)
	((time		time)
	 (time-duration	duration))
      ($add-duration time duration)))

  (define (add-duration! time duration)
    (define who 'add-duration!)
    (with-arguments-validation (who)
	((time		time)
	 (time-duration	duration))
      ($add-duration! time time duration)))

  (define (subtract-duration time duration)
    (define who 'subtract-duration)
    (with-arguments-validation (who)
	((time		time)
	 (time-duration	duration))
      ($subtract-duration time duration)))

  (define (subtract-duration! time duration)
    (define who 'subtract-duration!)
    (with-arguments-validation (who)
	((time		time)
	 (time-duration	duration))
      ($subtract-duration! time time duration)))

;;; --------------------------------------------------------------------

  (define ($time-difference time1 time2)
    ($time-difference! (make-time #f #f #f) time1 time2))

  (define ($time-difference! retval time1 time2)
    ($set-time-type! retval time-duration)
    (if ($time=? time1 time2)
	(begin
	  ($set-time-second!     retval 0)
	  ($set-time-nanosecond! retval 0))
      (receive (nanos secs)
	  ($nanoseconds->nanos+secs (- ($time->nanoseconds time1)
				       ($time->nanoseconds time2)))
	($set-time-second!     retval secs)
	($set-time-nanosecond! retval nanos)))
    retval)

;;; --------------------------------------------------------------------

  (define ($add-duration time duration)
    ($add-duration! (make-time ($time-type time) #f #f) time duration))

  (define ($add-duration! retval time1 duration)
    (let ((sec-plus  (+ ($time-second     time1) ($time-second     duration)))
	  (nsec-plus (+ ($time-nanosecond time1) ($time-nanosecond duration))))
      (let ((r (remainder nsec-plus NUMBER-OF-NANOSECONDS-IN-A-SECOND))
	    (q (quotient  nsec-plus NUMBER-OF-NANOSECONDS-IN-A-SECOND)))
	(if (negative? r)
	    (begin
	      ($set-time-second!     retval (+ sec-plus q -1))
	      ($set-time-nanosecond! retval (+ NUMBER-OF-NANOSECONDS-IN-A-SECOND r)))
	  (begin
	    ($set-time-second!     retval (+ sec-plus q))
	    ($set-time-nanosecond! retval r)))
	retval)))

;;; --------------------------------------------------------------------

  (define ($subtract-duration time duration)
    ($subtract-duration! (make-time ($time-type time) #f #f) time duration))

  (define ($subtract-duration! retval time1 duration)
    (let ((sec-minus  (- ($time-second     time1) ($time-second     duration)))
	  (nsec-minus (- ($time-nanosecond time1) ($time-nanosecond duration))))
      (let ((r (remainder nsec-minus NUMBER-OF-NANOSECONDS-IN-A-SECOND))
	    (q (quotient  nsec-minus NUMBER-OF-NANOSECONDS-IN-A-SECOND)))
	(if (negative? r)
	    (begin
	      ($set-time-second!     retval (- sec-minus q 1))
	      ($set-time-nanosecond! retval (+ NUMBER-OF-NANOSECONDS-IN-A-SECOND r)))
	  (begin
	    ($set-time-second!     retval (- sec-minus q))
	    ($set-time-nanosecond! retval r)))
	retval)))

;;; --------------------------------------------------------------------

  (define ($time->nanoseconds time)
    (+ (* ($time-second time) NUMBER-OF-NANOSECONDS-IN-A-SECOND)
       ($time-nanosecond time)))

  (define ($nanoseconds->nanos+secs nanoseconds)
    (let ((q (quotient  nanoseconds NUMBER-OF-NANOSECONDS-IN-A-SECOND))
	  (r (remainder nanoseconds NUMBER-OF-NANOSECONDS-IN-A-SECOND)))
      (if (negative? r)
	  (values (+ NUMBER-OF-NANOSECONDS-IN-A-SECOND r) (- q 1))
	(values r q))))

  #| end of module |# )


;;;; time conversion between types

(define (time-tai->time-utc time-in)
  (define who 'time-tai->time-utc)
  (with-arguments-validation (who)
      ((time-tai	time-in))
    ($time-tai->time-utc time-in)))

(define (time-tai->time-utc! time-in)
  (define who 'time-tai->time-utc!)
  (with-arguments-validation (who)
      ((time-tai	time-in))
    ($time-tai->time-utc! time-in time-in)))

(define (time-utc->time-tai time-in)
  (define who 'time-utc->time-tai)
  (with-arguments-validation (who)
      ((time-utc	time-in))
    ($time-utc->time-tai time-in)))

(define (time-utc->time-tai! time-in)
  (define who 'time-utc->time-tai!)
  (with-arguments-validation (who)
      ((time-utc	time-in))
    ($time-utc->time-tai! time-in time-in)))

;;; --------------------------------------------------------------------

(define ($time-tai->time-utc time-in)
  ($time-tai->time-utc! time-in (make-time #f #f #f)))

(define ($time-utc->time-tai time-in)
  ($time-utc->time-tai! time-in (make-time #f #f #f)))

(define ($time-tai->time-utc! time-in time-out)
  ($set-time-type!       time-out time-utc)
  (let ((tai-seconds	 ($time-second     time-in))
	(tai-nanoseconds ($time-nanosecond time-in)))
    (if (%tai-seconds-in-utc-bizarre-second? tai-seconds)
	(receive (utc-nanoseconds utc-seconds)
	    (%tai-time->bizarre-utc-time tai-nanoseconds tai-seconds)
	  ($set-time-second!     time-out utc-seconds)
	  ($set-time-nanosecond! time-out utc-nanoseconds))
      (begin
	($set-time-nanosecond! time-out tai-nanoseconds)
	(let ((delta (%tai->utc:leap-second-delta tai-seconds)))
	  ($set-time-second! time-out (- tai-seconds delta))))))
  time-out)

(define ($time-utc->time-tai! time-in time-out)
  ($set-time-type!       time-out time-tai)
  (let ((utc-seconds	 ($time-second     time-in))
	(utc-nanoseconds ($time-nanosecond time-in)))
    (if (%utc-seconds-in-utc-bizarre-second? utc-seconds)
	(receive (tai-nanoseconds tai-seconds)
	    (%bizarre-utc-time->tai-time utc-nanoseconds utc-seconds)
	  ($set-time-second!     time-out tai-seconds)
	  ($set-time-nanosecond! time-out tai-nanoseconds))
      (begin
	($set-time-nanosecond! time-out ($time-nanosecond time-in))
	(let ((delta (%utc->tai:leap-second-delta ($time-second time-in))))
	  ($set-time-second! time-out (+ ($time-second time-in) delta))))))
  time-out)

;;; --------------------------------------------------------------------

;;These depend on TIME-MONOTONIC having the same definition as TIME-TAI.

(define (time-monotonic->time-utc time-in)
  (define who 'time-monotonic->time-utc)
  (with-arguments-validation (who)
      ((time-monotonic	time-in))
    ($time-monotonic->time-utc time-in)))

(define ($time-monotonic->time-utc time-in)
  (let ((ntime ($copy-time time-in)))
    ($set-time-type! ntime time-tai)
    ($time-tai->time-utc! ntime ntime)))

(define (time-monotonic->time-utc! time-in)
  (define who 'time-monotonic->time-utc!)
  (with-arguments-validation (who)
      ((time-monotonic	time-in))
    ($time-monotonic->time-utc! time-in)))

(define ($time-monotonic->time-utc! time-in)
  ($set-time-type! time-in time-tai)
  ($time-tai->time-utc! time-in time-in))

;;; --------------------------------------------------------------------

(define (time-monotonic->time-tai time-in)
  (define who 'time-monotonic->time-tai)
  (with-arguments-validation (who)
      ((time-monotonic	time-in))
    ($time-monotonic->time-tai time-in)))

(define ($time-monotonic->time-tai time-in)
  (begin0-let ((ntime ($copy-time time-in)))
    ($set-time-type! ntime time-tai)))

(define (time-monotonic->time-tai! time-in)
  (define who 'time-monotonic->time-tai!)
  (with-arguments-validation (who)
      ((time-monotonic	time-in))
    ($time-monotonic->time-tai! time-in)))

(define ($time-monotonic->time-tai! time-in)
  ($set-time-type! time-in time-tai)
  time-in)

;;; --------------------------------------------------------------------

(define (time-utc->time-monotonic time-in)
  (define who 'time-utc->time-monotonic)
  (with-arguments-validation (who)
      ((time-utc	time-in))
    ($time-utc->time-monotonic time-in)))

(define ($time-utc->time-monotonic time-in)
  (begin0-let ((ntime ($time-utc->time-tai! time-in (make-time #f #f #f))))
    ($set-time-type! ntime time-monotonic)))

(define (time-utc->time-monotonic! time-in)
  (define who 'time-utc->time-monotonic!)
  (with-arguments-validation (who)
      ((time-utc	time-in))
    ($time-utc->time-monotonic! time-in)))

(define ($time-utc->time-monotonic! time-in)
  (begin0-let ((ntime ($time-utc->time-tai! time-in time-in)))
    (set-time-type! ntime time-monotonic)))

;;; --------------------------------------------------------------------

(define (time-tai->time-monotonic time-in)
  (define who 'time-tai->time-monotonic)
  (with-arguments-validation (who)
      ((time-tai	time-in))
    ($time-tai->time-monotonic time-in)))

(define ($time-tai->time-monotonic time-in)
  (begin0-let ((ntime ($copy-time time-in)))
    ($set-time-type! ntime time-monotonic)))

(define (time-tai->time-monotonic! time-in)
  (define who 'time-tai->time-monotonic!)
  (with-arguments-validation (who)
      ((time-tai	time-in))
    ($time-tai->time-monotonic! time-in)))

(define ($time-tai->time-monotonic! time-in)
  ($set-time-type! time-in time-monotonic)
  time-in)


;;;; date structure

(define-struct date
  (nanosecond second minute hour day month year zone-offset))

(module (true-make-date)
  (define who 'true-make-date)

  (define (true-make-date nanosecond second minute hour day month year zone-offset)
    (with-arguments-validation (who)
	((year			year)
	 (month			month)
	 (day			year month day)
	 (hour			hour)
	 (minute		minute)
	 (date-seconds		year month day hour minute second)
	 (date-nanoseconds	nanosecond)
	 (time-zone-offset	zone-offset))
      (make-date nanosecond second
		 minute hour day
		 month year
		 zone-offset)))

  (define-argument-validation (minute who obj)
    (and (fixnum? obj)
	 ($fx>= obj 0)
	 ($fx<= obj 59))
    (assertion-violation who "invalid minute count" obj))

  (define-argument-validation (hour who obj)
    (and (fixnum? obj)
	 ($fx>= obj 0)
	 ($fx<= obj 23))
    (assertion-violation who "invalid hour count" obj))

  (define-argument-validation (day who year month day)
    (and (fixnum? day)
	 ($fx>= day 0)
	 ($fx<= day (%max-day-count year month day)))
    (assertion-violation who "invalid day count" day))

  (define-argument-validation (month who obj)
    (and (fixnum? obj)
	 ($fx>= obj 0)
	 ($fx<= obj 12))
    (assertion-violation who "invalid month count" obj))

  (define-argument-validation (year who obj)
    (or (fixnum? obj)
	(bignum? obj))
    (assertion-violation who "invalid year count" obj))

  (define NUMBER-OF-DAYS-PER-MONTH/NON-LEAP-YEAR
    '#(0
       31	;Jan
       28	;Feb
       31	;Mar
       30	;Apr
       31	;May
       30	;Jun
       31	;Jul
       31	;Aug
       30	;Sep
       31	;Oct
       30	;Nov
       31))	;Dec

  (define NUMBER-OF-DAYS-PER-MONTH/LEAP-YEAR
    '#(0
       31	;Jan
       29	;Feb
       31	;Mar
       30	;Apr
       31	;May
       30	;Jun
       31	;Jul
       31	;Aug
       30	;Sep
       31	;Oct
       30	;Nov
       31))	;Dec

  (define (%max-day-count year month day)
    ($vector-ref (if (%leap-year? year)
		     NUMBER-OF-DAYS-PER-MONTH/LEAP-YEAR
		   NUMBER-OF-DAYS-PER-MONTH/NON-LEAP-YEAR)
		 month))

  #| end of module: true-make-date |# )

;;; --------------------------------------------------------------------

(define-argument-validation (date who obj)
  (date? obj)
  (assertion-violation who "expected date object as argument" obj))

;;; --------------------------------------------------------------------

(define current-date
  (case-lambda
   (()
    (current-date (%local-tz-offset)))
   ((tz-offset)
    (define who 'current-date)
    (with-arguments-validation (who)
	((time-zone-offset	tz-offset))
      ($time-utc->date (current-time time-utc) tz-offset)))))


;;;; time to date conversion

(define time-tai->date
  (case-lambda
   ((time)
    (time-tai->date time (%local-tz-offset)))
   ((time tz-offset)
    (define who 'time-tai->date)
    (with-arguments-validation (who)
	((time-tai		time)
	 (time-zone-offset	tz-offset))
      ($time-tai->date time tz-offset)))))

(define ($time-tai->date time tz-offset)
  (if (%tai-before-leap-second? ($time-second time))
      ;;If  it's *right*  before the  leap,  we need  to pretend  to
      ;;subtract a second ...
      (let* ((time^ (let ((T ($time-tai->time-utc time)))
		      ($subtract-duration! T (make-time time-duration 0 1))))
	     (date ($time-utc->date time^ tz-offset time-utc)))
	($set-date-second! date 60)
	date)
    ($time-utc->date ($time-tai->time-utc time) tz-offset)))

(module (%tai-before-leap-second?)

  (define (%tai-before-leap-second? second)
    (%find (lambda (x)
	     (= second (- (+ (car x) (cdr x)) 1)))
	   LEAP-SECONDS-TABLE))

  (define (%find proc l)
    (cond ((null? l)
	   #f)
	  ((proc (car l))
	   #t)
	  (else
	   (%find proc (cdr l)))))

  #| end of module |# )

;;; --------------------------------------------------------------------

(define time-utc->date
  (case-lambda
   ((time)
    (time-utc->date time (%local-tz-offset)))
   ((time tz-offset)
    (define who 'time-utc->date)
    (with-arguments-validation (who)
	((time-utc		time)
	 (time-zone-offset	tz-offset))
      ($time-utc->date time tz-offset)))))

(define time-monotonic->date
  ;;Again, time-monotonic is the same as time tai.
  ;;
  (case-lambda
   ((time)
    (time-monotonic->date time (%local-tz-offset)))
   ((time tz-offset)
    (define who 'time-monotonic->date)
    (with-arguments-validation (who)
	((time-monotonic	time)
	 (time-zone-offset	tz-offset))
      ($time-utc->date time tz-offset)))))

(define ($time-utc->date time tz-offset)
  (receive (secs date month year)
      (%decode-julian-day-number
       (%time->julian-day-number ($time-second time) tz-offset))
    (let* ((hours    (quotient  secs (* 60 60)))
	   (rem      (remainder secs (* 60 60)))
	   (minutes  (quotient  rem 60))
	   (seconds  (remainder rem 60)))
      (true-make-date ($time-nanosecond time)
		 seconds
		 minutes
		 hours
		 date
		 month
		 year
		 tz-offset))))


;;;; date to time conversion

(define (date->time-utc date)
  (define who 'date->time-utc)
  (with-arguments-validation (who)
      ((date	date))
    ($date->time-utc date)))

(define ($date->time-utc date)
  (let ((nanosecond	($date-nanosecond  date))
	(second		($date-second      date))
	(minute		($date-minute      date))
	(hour		($date-hour        date))
	(day		($date-day         date))
	(month		($date-month       date))
	(year		($date-year        date))
	(offset		($date-zone-offset date)))
    (let ((jdays (- (%encode-julian-day-number day month year)
		    TAI-EPOCH-IN-JULIAN-DAYS)))
      (make-time time-utc nanosecond
		 (+ (* (- jdays 1/2) 24 60 60)
		    (* hour 60 60)
		    (* minute 60)
		    second
		    (- offset))))))

;;; --------------------------------------------------------------------

(define (date->time-tai date)
  (define who 'date->time-tai)
  (with-arguments-validation (who)
      ((date	date))
    ($date->time-tai date)))

(define ($date->time-tai date)
  (if (= ($date-second date) 60)
      (let ((T (time-utc->time-tai! ($date->time-utc date))))
	($subtract-duration! T (make-time time-duration 0 1)))
    ($time-utc->time-tai! ($date->time-utc date))))

;;; --------------------------------------------------------------------

(define (date->time-monotonic date)
  (define who 'date->time-monotonic)
  (with-arguments-validation (who)
      ((date	date))
    ($date->time-monotonic date)))

(define ($date->time-monotonic date)
  ($time-utc->time-monotonic! ($date->time-utc date)))


;;;; basic julian day functions

(define (current-julian-day)
  ($time-utc->julian-day (current-time time-utc)))

(define (current-modified-julian-day)
  ($time-utc->modified-julian-day (current-time time-utc)))

(define (%modified-julian-day->julian-day mjdn)
  (+ mjdn JDN-MJDN))

(define (%julian-day->modified-julian-day jdn)
  (- jdn JDN-MJDN))


;;;; misc routines

;; gives the julian day which starts at noon.
(define (%encode-julian-day-number day month year)
  (let* ((a (quotient (- 14 month) 12))
	 (y (- (- (+ year 4800) a) (if (negative? year) -1 0)))
	 (m (- (+ month (* 12 a)) 3)))
    (+ day
       (quotient (+ (* 153 m) 2) 5)
       (* 365 y)
       (quotient y 4)
       (- (quotient y 100))
       (quotient y 400)
       -32045)))

;; gives the seconds/date/month/year
(define (%decode-julian-day-number jdn)
  (let* ((days (truncate jdn))
	 (a (+ days 32044))
	 (b (quotient (+ (* 4 a) 3) 146097))
	 (c (- a (quotient (* 146097 b) 4)))
	 (d (quotient (+ (* 4 c) 3) 1461))
	 (e (- c (quotient (* 1461 d) 4)))
	 (m (quotient (+ (* 5 e) 2) 153))
	 (y (+ (* 100 b) d -4800 (quotient m 10))))
    (values ; seconds date month year
     (* (- jdn days) NUMBER-OF-SECONDS-IN-A-DAY)
     (+ e (- (quotient (+ (* 153 m) 2) 5)) 1)
     (+ m 3 (* -12 (quotient m 10)))
     (if (>= 0 y) (- y 1) y))
    ))

(define (%time->julian-day-number seconds tz-offset)
  ;;Special function: ignores nanoseconds.
  ;;
  (+ (/ (+ seconds tz-offset NUMBER-OF-SECONDS-IN-HALF-A-DAY)
	NUMBER-OF-SECONDS-IN-A-DAY)
     TAI-EPOCH-IN-JULIAN-DAYS))

(define (%leap-year? year)
  (or (= (modulo year 400) 0)
      (and (= (modulo year 4) 0)
	   (not (= (modulo year 100) 0)))))

;;; --------------------------------------------------------------------

(define (date-year-day date)
  (define who 'date-year-day)
  (with-arguments-validation (who)
      ((date	date))
    (%year-day ($date-day date) ($date-month date) ($date-year date))))

(define (%year-day day month year)
  (define who 'date-year-day)
  (define MONTH-ASSOC
    '((0 . 0)		(1 . 31)	(2 . 59)
      (3 . 90)		(4 . 120)	(5 . 151)
      (6 . 181)		(7 . 212)	(8 . 243)
      (9 . 273)		(10 . 304)	(11 . 334)))
  (let ((days-pr (assoc (- month 1) MONTH-ASSOC)))
    (if (not days-pr)
	(error who "invalid month specification" month))
    (if (and (%leap-year? year) (> month 2))
	(+ day (cdr days-pr) 1)
      (+ day (cdr days-pr)))))

;;; --------------------------------------------------------------------

(define (date-week-day date)
  (define who 'date-week-day)
  (with-arguments-validation (who)
      ((date	date))
    (%week-day ($date-day date) ($date-month date) ($date-year date))))

(define (%week-day day month year)
  ;;From calendar FAQ.
  ;;
  (let* ((a (quotient (- 14 month) 12))
	 (y (- year a))
	 (m (+ month (* 12 a) -2)))
    (modulo (+ day y (quotient y 4) (- (quotient y 100))
	       (quotient y 400) (quotient (* 31 m) 12))
	    7)))

;;; --------------------------------------------------------------------

(define (date-week-number date day-of-week-starting-week)
  (define who 'date-week-number)
  (with-arguments-validation (who)
      ((date	date)
       (fixnum	day-of-week-starting-week))
    (quotient (- (date-year-day date)
		 (%days-before-first-week date day-of-week-starting-week))
	      7)))

(define (%days-before-first-week date day-of-week-starting-week)
  (let* ((first-day  (true-make-date 0 0 0 0
				1
				1
				($date-year date)
				#f))
	 (fdweek-day (date-week-day first-day)))
    (modulo (- day-of-week-starting-week fdweek-day)
            7)))


;;;; conversion from time objects to julian day numbers

(define (time-utc->julian-day time)
  (define who 'time-utc->julian-day)
  (with-arguments-validation (who)
      ((time-utc	time))
    ($time-utc->julian-day time)))

(define ($time-utc->julian-day time)
  (+ (/ (+ ($time-second time)
	   (/ ($time-nanosecond time)
	      NUMBER-OF-NANOSECONDS-IN-A-SECOND))
	NUMBER-OF-SECONDS-IN-A-DAY)
     TAI-EPOCH-IN-JULIAN-DAYS))

;;; --------------------------------------------------------------------

(define (time-utc->modified-julian-day time)
  (define who 'time-utc->modified-julian-day)
  (with-arguments-validation (who)
      ((time-utc	time))
    ($time-utc->modified-julian-day time)))

(define ($time-utc->modified-julian-day time)
  (%julian-day->modified-julian-day ($time-utc->julian-day time)))

;;; --------------------------------------------------------------------

(define (time-tai->julian-day time)
  (define who 'time-tai->julian-day)
  (with-arguments-validation (who)
      ((time-tai	time))
    ($time-tai->julian-day time)))

(define ($time-tai->julian-day time)
  (+ (/ (+ (- ($time-second time)
	      (%utc->tai:leap-second-delta ($time-second time)))
	   (/ ($time-nanosecond time)
	      NUMBER-OF-NANOSECONDS-IN-A-SECOND))
	NUMBER-OF-SECONDS-IN-A-DAY)
     TAI-EPOCH-IN-JULIAN-DAYS))

;;; --------------------------------------------------------------------

(define (time-tai->modified-julian-day time)
  (define who 'time-tai->modified-julian-day)
  (with-arguments-validation (who)
      ((time-tai	time))
    ($time-tai->modified-julian-day time)))

(define ($time-tai->modified-julian-day time)
  (%julian-day->modified-julian-day ($time-tai->julian-day time)))

;;; --------------------------------------------------------------------

(define (time-monotonic->julian-day time)
  ;;This is the same as TIME-TAI->JULIAN-DAY.
  ;;
  (define who 'time-monotonic->julian-day)
  (with-arguments-validation (who)
      ((time-monotonic	time))
    ($time-monotonic->julian-day time)))

(define ($time-monotonic->julian-day time)
  (+ (/ (+ (- ($time-second time)
	      (%utc->tai:leap-second-delta ($time-second time)))
	   (/ ($time-nanosecond time)
	      NUMBER-OF-NANOSECONDS-IN-A-SECOND))
	NUMBER-OF-SECONDS-IN-A-DAY)
     TAI-EPOCH-IN-JULIAN-DAYS))

;;; --------------------------------------------------------------------

(define (time-monotonic->modified-julian-day time)
  (define who 'time-monotonic->modified-julian-day)
  (with-arguments-validation (who)
      ((time-monotonic	time))
    ($time-monotonic->modified-julian-day time)))

(define ($time-monotonic->modified-julian-day time)
  (%julian-day->modified-julian-day ($time-monotonic->julian-day time)))


;;;; conversion from julian day numbers to time objects

(define (julian-day->time-utc jdn)
  (define who 'julian-day->time-utc)
  (with-arguments-validation (who)
      ((julian-day-number	jdn))
    ($julian-day->time-utc jdn)))

(define ($julian-day->time-utc jdn)
  (receive (nanos secs)
      ($nanoseconds->nanos+secs (* NUMBER-OF-NANOSECONDS-IN-A-SECOND
				   NUMBER-OF-SECONDS-IN-A-DAY
				   (- jdn TAI-EPOCH-IN-JULIAN-DAYS)))
    (make-time time-utc nanos secs)))

;;; --------------------------------------------------------------------

(define (julian-day->time-tai jdn)
  (define who 'julian-day->time-tai)
  (with-arguments-validation (who)
      ((julian-day-number	jdn))
    ($julian-day->time-tai jdn)))

(define ($julian-day->time-tai jdn)
  ($time-utc->time-tai! ($julian-day->time-utc jdn)))

;;; --------------------------------------------------------------------

(define (julian-day->time-monotonic jdn)
  (define who 'julian-day->time-monotonic)
  (with-arguments-validation (who)
      ((julian-day-number	jdn))
    ($julian-day->time-monotonic jdn)))

(define ($julian-day->time-monotonic jdn)
  ($time-utc->time-monotonic! ($julian-day->time-utc jdn)))

;;; --------------------------------------------------------------------

(define (modified-julian-day->time-utc mjdn)
  (define who 'modified-julian-day->time-utc)
  (with-arguments-validation (who)
      ((modified-julian-day-number	mjdn))
    ($modified-julian-day->time-utc mjdn)))

(define ($modified-julian-day->time-utc mjdn)
  ($julian-day->time-utc (%modified-julian-day->julian-day mjdn)))

;;; --------------------------------------------------------------------

(define (modified-julian-day->time-tai mjdn)
  (define who 'modified-julian-day->time-tai)
  (with-arguments-validation (who)
      ((modified-julian-day-number	mjdn))
    ($modified-julian-day->time-tai mjdn)))

(define ($modified-julian-day->time-tai mjdn)
  ($julian-day->time-tai (%modified-julian-day->julian-day mjdn)))

;;; --------------------------------------------------------------------

(define (modified-julian-day->time-monotonic mjdn)
  (define who 'modified-julian-day->time-monotonic)
  (with-arguments-validation (who)
      ((modified-julian-day-number	mjdn))
    ($modified-julian-day->time-monotonic mjdn)))

(define ($modified-julian-day->time-monotonic mjdn)
  ($julian-day->time-monotonic (%modified-julian-day->julian-day mjdn)))


;;;; conversion from julian day numbers to date objects

(define julian-day->date
  (case-lambda
   ((jdn)
    (julian-day->date jdn (%local-tz-offset)))
   ((jdn tz-offset)
    (define who 'julian-day->date)
    (with-arguments-validation (who)
	((julian-day-number	jdn)
	 (time-zone-offset	tz-offset))
      ($julian-day->date jdn tz-offset)))))

(define ($julian-day->date jdn tz-offset)
  ($time-utc->date ($julian-day->time-utc jdn) tz-offset))

;;; --------------------------------------------------------------------

(define modified-julian-day->date
  (case-lambda
   ((mjdn)
    (modified-julian-day->date mjdn (%local-tz-offset)))
   ((mjdn tz-offset)
    (define who 'modified-julian-day->date)
    (with-arguments-validation (who)
	((modified-julian-day-number	mjdn)
	 (time-zone-offset		tz-offset))
      ($modified-julian-day->date mjdn tz-offset)))))

(define ($modified-julian-day->date mjdn tz-offset)
  ($julian-day->date (%modified-julian-day->julian-day mjdn) tz-offset))


;;;; conversion from date objects to julian day numbers

(define (date->julian-day date)
  (define who 'date->julian-day)
  (with-arguments-validation (who)
      ((date	date))
    ($date->julian-day date)))

(define ($date->julian-day date)
  (let ((nanosecond	($date-nanosecond date))
	(second		($date-second		date))
	(minute		($date-minute		date))
	(hour		($date-hour		date))
	(day		($date-day		date))
	(month		($date-month		date))
	(year		($date-year		date))
	(offset		($date-zone-offset	date)))
    (+ (%encode-julian-day-number day month year)
       -1/2
       (+ (/ (+ (* hour 60 60)
                (* minute 60)
                second
                (/ nanosecond NUMBER-OF-NANOSECONDS-IN-A-SECOND)
                (- offset))
             NUMBER-OF-SECONDS-IN-A-DAY)))))

;;; --------------------------------------------------------------------

(define (date->modified-julian-day date)
  (define who 'date->modified-julian-day)
  (with-arguments-validation (who)
      ((date	date))
    ($date->modified-julian-day date)))

(define ($date->modified-julian-day date)
  (%julian-day->modified-julian-day ($date->julian-day date)))


;;;; string to date

(module (string->date)
  (define who 'string->date)

  (define (string->date input-string template-string)
    (with-arguments-validation (who)
	((string	input-string)
	 (string	template-string))
      (let ((newdate (true-make-date 0 0 0 0 #f #f #f (%local-tz-offset))))
	(%string->date newdate
		       0
		       template-string
		       ($string-length template-string)
		       (open-input-string input-string)
		       template-string)
	(if (%all-fields-set-in-date? newdate)
	    newdate
	  (error who "bad date format string, incomplete date read" newdate template-string)))))

  (define (%all-fields-set-in-date? date)
    ;;Return true if all the fields in DATE have been set.
    ;;
    (and ($date-nanosecond	date)
	 ($date-second		date)
	 ($date-minute		date)
	 ($date-hour		date)
	 ($date-day		date)
	 ($date-month		date)
	 ($date-year		date)
	 ($date-zone-offset	date)))

  (define (%string->date date index format-string str-len port template-string)
    (unless (>= index str-len)
      (let ((current-char (string-ref format-string index)))
	(if (not (char=? current-char #\~))
	    (let ((port-char (read-char port)))
	      (if (or (eof-object? port-char)
		      (not (char=? current-char port-char)))
		  (%error-bad-template template-string))
	      (%string->date date (+ index 1) format-string str-len port template-string))
	  ;;Otherwise, it's an escape, we hope.
	  (if (> (+ index 1) str-len)
	      (%error-bad-template template-string)
	    (let* ((format-char (string-ref format-string (+ index 1)))
		   (format-info (assoc format-char %READ-DIRECTIVES)))
	      (if (not format-info)
		  (%error-bad-template template-string)
		(begin
		  (let ((skipper (cadr format-info))
			(reader  (caddr format-info))
			(actor   (cadddr format-info)))
		    (%skip-until port skipper template-string)
		    (let ((val (reader port)))
		      (if (eof-object? val)
			  (%error-bad-template template-string)
			(actor val date)))
		    (%string->date date (+ index 2) format-string
				   str-len port template-string))))))))))

  (define (%skip-until port skipper template-string)
    (let ((ch (peek-char port)))
      (if (eof-object? ch)
	  (error who "bad date format string" template-string)
	(unless (skipper ch)
	  (read-char port)
	  (%skip-until port skipper template-string)))))

  (define (%make-integer-reader upto)
    (lambda (port)
      (%integer-reader upto port)))

  (define (%make-fractional-integer-reader upto)
    (lambda (port)
      (%fractional-integer-reader upto port)))

  (define (%make-locale-reader indexer)
    (lambda (port)
      (%locale-reader port indexer)))

  (define (%make-char-id-reader char)
    (lambda (port)
      (if (char=? char (read-char port))
	  char
	(error who "bad date template string, invalid character match"))))

  (define (%make-integer-exact-reader n)
    (lambda (port)
      (%integer-reader-exact n port)))

  (define (%char->int ch)
    (case-chars ch
      ((#\0) 0)
      ((#\1) 1)
      ((#\2) 2)
      ((#\3) 3)
      ((#\4) 4)
      ((#\5) 5)
      ((#\6) 6)
      ((#\7) 7)
      ((#\8) 8)
      ((#\9) 9)
      (else
       (error who "bad date template string, non-integer character" ch))))

  (define (%integer-reader upto port)
    ;;Read an integer upto n characters long  on port; upto -> #f if any
    ;;length.
    (define (accum-int port accum nchars)
      (let ((ch (peek-char port)))
	(if (or (eof-object? ch)
		(not (char-numeric? ch))
		(and upto (>= nchars upto)))
	    accum
          (accum-int port (+ (* accum 10) (%char->int (read-char port))) (+ nchars 1)))))
    (accum-int port 0 0))

  (define (%fractional-integer-reader upto port)
    ;;Read an fractional integer upto n characters long on port; upto ->
    ;;#f if any length.
    ;;
    ;;The  return  value  is  normalized to  upto  decimal  places.  For
    ;;example, if  upto is 9  and the string  read is "123",  the return
    ;;value is 123000000.
    ;;
    (define (accum-int port accum nchars)
      (let ((ch (peek-char port)))
	(if (or (eof-object? ch)
		(not (char-numeric? ch))
		(and upto (>= nchars  upto)))
	    (* accum (expt 10 (- upto nchars)))
	  (accum-int port (+ (* accum 10) (%char->int (read-char port))) (+ nchars 1)))))
    (accum-int port 0 0))

  (define (%integer-reader-exact n port)
    ;;Read exactly N characters and convert to integer; could be padded.
    ;;
    (let ((padding-ok #t))
      (define (accum-int port accum nchars)
	(let ((ch (peek-char port)))
	  (cond ((>= nchars n)
		 accum)
		((eof-object? ch)
		 (error who "bad date template string, premature ending to integer read"))
		((char-numeric? ch)
		 (set! padding-ok #f)
		 (accum-int port
			    (+ (* accum 10) (%char->int (read-char port)))
			    (+ nchars 1)))
		(padding-ok
		 (read-char port) ;consume padding
		 (accum-int port accum (+ nchars 1)))
		(else
		 ;;padding where it shouldn't be
		 (error who
		   "bad date template string, non-numeric characters in integer read.")))))
      (accum-int port 0 0)))

  (define (%zone-reader port)
    (let ((offset    0)
	  (positive? #f))
      (let ((ch (read-char port)))
	(when (eof-object? ch)
	  (%error-invalid-time-zone-pm ch))
	(if (or ($char= ch #\Z)
		($char= ch #\z))
	    0
	  (begin
	    (case-chars ch
	      ((#\+)
	       (set! positive? #t))
	      ((#\-)
	       (set! positive? #f))
	      (else
	       (%error-invalid-time-zone-pm ch)))
	    (let ((ch (read-char port)))
	      (if (eof-object? ch)
		  (%error-invalid-time-zone-number ch)
		(set! offset (* (%char->int ch) 10 60 60))))
	    (let ((ch (read-char port)))
	      (if (eof-object? ch)
		  (%error-invalid-time-zone-number ch)
		(set! offset (+ offset (* (%char->int ch) 60 60)))))
	    (let ((ch (read-char port)))
	      (if (eof-object? ch)
		  (%error-invalid-time-zone-number ch)
		(set! offset (+ offset (* (%char->int ch) 10 60)))))
	    (let ((ch (read-char port)))
	      (if (eof-object? ch)
		  (%error-invalid-time-zone-number ch)
		(set! offset (+ offset (* (%char->int ch) 60)))))
	    (if positive?
		offset
	      (- offset)))))))

  (define (%locale-reader port indexer)
    ;;Looking at a char, read the  char string, run thru indexer, return
    ;;index.
    ;;
    (let ((string-port (open-output-string)))
      (define (read-char-string)
	(let ((ch (peek-char port)))
	  (if (char-alphabetic? ch)
	      (begin
		(write-char (read-char port) string-port)
		(read-char-string))
	    (get-output-string string-port))))
      (let* ((str   (read-char-string))
	     (index (indexer str)))
	(or index
	    (error who "bad date template string")))))

  (define (%natural-year n)
    ;;Given a 'two digit' number, find the year within 50 years +/-.
    (let* ((current-year    ($date-year (current-date)))
	   (current-century (* (quotient current-year 100) 100)))
      (cond ((>= n 100)
	     n)
	    ((<  n 0)
	     n)
	    ((<= (- (+ current-century n) current-year) 50)
	     (+ current-century n))
	    (else
	     (+ (- current-century 100) n)))))

;;; --------------------------------------------------------------------

  (module (%locale-abbr-weekday->index
	   %locale-long-weekday->index
	   %locale-abbr-month->index
	   %locale-long-month->index)

    (define (%locale-abbr-weekday->index string)
      (%vector-find string LOCALE-ABBR-WEEKDAY-VECTOR string=?))

    (define (%locale-long-weekday->index string)
      (%vector-find string LOCALE-LONG-WEEKDAY-VECTOR string=?))

    (define (%locale-abbr-month->index string)
      (%vector-find string LOCALE-ABBR-MONTH-VECTOR string=?))

    (define (%locale-long-month->index string)
      (%vector-find string LOCALE-LONG-MONTH-VECTOR string=?))

    (define (%vector-find needle haystack comparator)
      (let ((len (vector-length haystack)))
	(define (%vector-find-int index)
	  (cond ((>= index len)
		 #f)
		((comparator needle ($vector-ref haystack index))
		 index)
		(else
		 (%vector-find-int (+ index 1)))))
	(%vector-find-int 0)))

    #| end of module |# )


;;; --------------------------------------------------------------------

  (define (%error-bad-template template-string)
    (error who "bad date format string" template-string))

  (define (%error-invalid-time-zone-number ch)
    (error who "bad date template string, invalid time zone number" ch))

  (define (%error-invalid-time-zone-pm ch)
    (error who "bad date template string, invalid time zone +/-" ch))

;;; --------------------------------------------------------------------

  (define %READ-DIRECTIVES
    ;;A List of formatted read directives.  Each entry is a list.
    ;;
    ;;1. The character  directive; a procedure, which  takes a character
    ;;   as input & returns.
    ;;
    ;;2. #t as soon  as a character on the input  port is acceptable for
    ;;   input.
    ;;
    ;;3. A port reader procedure that knows how to read the current port
    ;;   for a value. Its one parameter is the port.
    ;;
    ;;4. A  action procedure, that takes  the value (from 3.)   and some
    ;;   object (here, always the  date) and (probably) side-effects it.
    ;;   In some cases (e.g., ~A) the action is to do nothing.
    ;;
    (let ((ireader4		(%make-integer-reader 4))
	  (ireader2		(%make-integer-reader 2))
	  (fireader9		(%make-fractional-integer-reader 9))
	  (ireaderf		(%make-integer-reader #f))
	  (eireader2		(%make-integer-exact-reader 2))
	  (eireader4		(%make-integer-exact-reader 4))
	  (locale-reader-abbr-weekday (%make-locale-reader %locale-abbr-weekday->index))
	  (locale-reader-long-weekday (%make-locale-reader %locale-long-weekday->index))
	  (locale-reader-abbr-month   (%make-locale-reader %locale-abbr-month->index))
	  (locale-reader-long-month   (%make-locale-reader %locale-long-month->index))
	  (char-fail		(lambda (ch) #t))
	  (do-nothing		(lambda (val object) (values))))
      (list
       (list #\~ char-fail (%make-char-id-reader #\~) do-nothing)
       (list #\a char-alphabetic? locale-reader-abbr-weekday do-nothing)
       (list #\A char-alphabetic? locale-reader-long-weekday do-nothing)
       (list #\b char-alphabetic? locale-reader-abbr-month
	     (lambda (val object)
	       ($set-date-month! object val)))
       (list #\B char-alphabetic? locale-reader-long-month
	     (lambda (val object)
	       ($set-date-month! object val)))
       (list #\d char-numeric? ireader2 (lambda (val object)
					  ($set-date-day!
					   object val)))
       (list #\e char-fail eireader2 (lambda (val object)
				       ($set-date-day! object val)))
       (list #\h char-alphabetic? locale-reader-abbr-month
	     (lambda (val object)
	       ($set-date-month! object val)))
       (list #\H char-numeric? ireader2 (lambda (val object)
					  ($set-date-hour! object val)))
       (list #\k char-fail eireader2 (lambda (val object)
				       ($set-date-hour! object val)))
       (list #\m char-numeric? ireader2 (lambda (val object)
					  ($set-date-month! object val)))
       (list #\M char-numeric? ireader2 (lambda (val object)
					  ($set-date-minute!
					   object val)))
       (list #\N char-numeric? fireader9 (lambda (val object)
					   ($set-date-nanosecond! object val)))
       (list #\S char-numeric? ireader2 (lambda (val object)
					  ($set-date-second! object val)))
       (list #\y char-fail eireader2
	     (lambda (val object)
	       ($set-date-year! object (%natural-year val))))
       (list #\Y char-numeric? ireader4 (lambda (val object)
					  ($set-date-year! object val)))
       (list #\z (lambda (c)
		   (or (char=? c #\Z)
		       (char=? c #\z)
		       (char=? c #\+)
		       (char=? c #\-)))
	     %zone-reader (lambda (val object)
			    ($set-date-zone-offset! object val)))
       )))

  #| end of module |# )


;;;; date to string

(module (date->string)
  (define who 'date->string)

  (define date->string
    (case-lambda
     ((date)
      (date->string date "~c"))
     ((date format-string)
      (with-arguments-validation (who)
	  ((date	date)
	   (string	format-string))
	(receive (port getter)
	    (open-string-output-port)
	  (%date-printer date 0 format-string ($string-length format-string) port)
	  (getter))))))

  (define (%date-printer date index format-string str.len port)
    (unless (>= index str.len)
      (let ((current-char ($string-ref format-string index)))
	(if (not ($char= current-char #\~))
	    (begin
	      (display current-char port)
	      (%date-printer date (+ index 1) format-string str.len port))
	  (if (= (+ index 1) str.len)
	      (%error-bad-template format-string)
	    (let ((pad-char? (string-ref format-string (+ index 1))))
	      (case-chars pad-char?
		((#\-)
		 (if (= (+ index 2) str.len)
		     (%error-bad-template format-string)
		   (let ((formatter (%get-formatter (string-ref format-string (+ index 2)))))
		     (if (not formatter)
			 (%error-bad-template format-string)
		       (begin
			 (formatter date #f port)
			 (%date-printer date (+ index 3) format-string str.len port))))))

		((#\_)
		 (if (= (+ index 2) str.len)
		     (%error-bad-template format-string)
		   (let ((formatter (%get-formatter (string-ref format-string (+ index 2)))))
		     (if (not formatter)
			 (%error-bad-template format-string)
		       (begin
			 (formatter date #\space port)
			 (%date-printer date (+ index 3) format-string str.len port))))))

		(else
		 (let ((formatter (%get-formatter (string-ref format-string (+ index 1)))))
		   (if (not formatter)
		       (%error-bad-template format-string)
		     (begin
		       (formatter date #\0 port)
		       (%date-printer date (+ index 2) format-string str.len port))))))))))))

  (define (%get-formatter char)
    (let ((associated (assoc char %DIRECTIVES)))
      (if associated (cdr associated) #f)))

  (define %DIRECTIVES
    ;;A table  of output formatting  directives.  The first time  is the
    ;;format char.   The second is  a procedure  that takes the  date, a
    ;;padding character (which might be #f), and the output port.
    ;;
    (list
     (cons #\~ (lambda (date pad-with port) (display #\~ port)))

     (cons #\a (lambda (date pad-with port)
		 (display (%locale-abbr-weekday (date-week-day date))
			  port)))
     (cons #\A (lambda (date pad-with port)
		 (display (%locale-long-weekday (date-week-day date))
			  port)))
     (cons #\b (lambda (date pad-with port)
		 (display (%locale-abbr-month ($date-month date))
			  port)))
     (cons #\B (lambda (date pad-with port)
		 (display (%locale-long-month ($date-month date))
			  port)))
     (cons #\c (lambda (date pad-with port)
		 (display (date->string date LOCALE-DATE-TIME-FORMAT) port)))
     (cons #\d (lambda (date pad-with port)
		 (display (%padding (date-day date)
				    #\0 2)
			  port)))
     (cons #\D (lambda (date pad-with port)
		 (display (date->string date "~m/~d/~y") port)))
     (cons #\e (lambda (date pad-with port)
		 (display (%padding (date-day date)
				    #\space 2)
			  port)))
     (cons #\f (lambda (date pad-with port)
		 (if (> ($date-nanosecond date)
			NUMBER-OF-NANOSECONDS-IN-A-SECOND)
		     (display (%padding (+ ($date-second date) 1)
					pad-with 2)
			      port)
		   (display (%padding ($date-second date)
				      pad-with 2)
			    port))
		 (display LOCALE-NUMBER-SEPARATOR port)
		 (display (%fractional-part (/ ($date-nanosecond date)
						 NUMBER-OF-NANOSECONDS-IN-A-SECOND))
			  port)))
     (cons #\h (lambda (date pad-with port)
		 (display (date->string date "~b") port)))
     (cons #\H (lambda (date pad-with port)
		 (display (%padding ($date-hour date)
				    pad-with 2)
			  port)))
     (cons #\I (lambda (date pad-with port)
		 (let ((hr ($date-hour date)))
		   (if (> hr 12)
		       (display (%padding (- hr 12)
					  pad-with 2)
				port)
		     (display (%padding hr
					pad-with 2)
			      port)))))
     (cons #\j (lambda (date pad-with port)
		 (display (%padding (date-year-day date)
				    pad-with 3)
			  port)))
     (cons #\k (lambda (date pad-with port)
		 (display (%padding ($date-hour date) #\0 2) port)))
     (cons #\l (lambda (date pad-with port)
		 (let ((hr (if (> ($date-hour date) 12)
			       (- ($date-hour date) 12) ($date-hour date))))
		   (display (%padding hr  #\space 2)
			    port))))
     (cons #\m (lambda (date pad-with port)
		 (display (%padding ($date-month date) pad-with 2)
			  port)))
     (cons #\M (lambda (date pad-with port)
		 (display (%padding ($date-minute date)
				    pad-with 2)
			  port)))
     (cons #\n (lambda (date pad-with port)
		 (newline port)))
     (cons #\N (lambda (date pad-with port)
		 (display (%padding ($date-nanosecond date)
				    pad-with 9)
			  port)))
     (cons #\p (lambda (date pad-with port)
		 (display (%locale-am/pm ($date-hour date)) port)))
     (cons #\r (lambda (date pad-with port)
		 (display (date->string date "~I:~M:~S ~p") port)))
     (cons #\s (lambda (date pad-with port)
		 (display (time-second (date->time-utc date)) port)))
     (cons #\S (lambda (date pad-with port)
		 (if (> ($date-nanosecond date)
			NUMBER-OF-NANOSECONDS-IN-A-SECOND)
		     (display (%padding (+ ($date-second date) 1) pad-with 2) port)
                   (display (%padding ($date-second date) pad-with 2) port))))
     (cons #\t (lambda (date pad-with port)
		 (display (integer->char 9) port)))
     (cons #\T (lambda (date pad-with port)
		 (display (date->string date "~H:~M:~S") port)))
     (cons #\U (lambda (date pad-with port)
		 (if (> (%days-before-first-week date 0) 0)
		     (display (%padding (+ (date-week-number date 0) 1)
					#\0 2) port)
		   (display (%padding (date-week-number date 0)
				      #\0 2) port))))
     (cons #\V (lambda (date pad-with port)
		 (display (%padding (date-week-number date 1)
				    #\0 2) port)))
     (cons #\w (lambda (date pad-with port)
		 (display (date-week-day date) port)))
     (cons #\x (lambda (date pad-with port)
		 (display (date->string date LOCALE-SHORT-DATE-FORMAT) port)))
     (cons #\X (lambda (date pad-with port)
		 (display (date->string date LOCALE-TIME-FORMAT) port)))
     (cons #\W (lambda (date pad-with port)
		 (if (> (%days-before-first-week date 1) 0)
		     (display (%padding (+ (date-week-number date 1) 1)
					#\0 2) port)
		   (display (%padding (date-week-number date 1)
				      #\0 2) port))))
     (cons #\y (lambda (date pad-with port)
		 (display (%padding (%last-n-digits (date-year date) 2)
				    pad-with 2)
			  port)))
     (cons #\Y (lambda (date pad-with port)
		 (display (%padding (date-year date) pad-with 4) port)))
     (cons #\z (lambda (date pad-with port)
		 (%tz-printer (date-zone-offset date) port)))
     (cons #\Z (lambda (date pad-with port)
		 (%locale-print-time-zone date port)))
     (cons #\1 (lambda (date pad-with port)
		 (display (date->string date "~Y-~m-~d") port)))
     (cons #\2 (lambda (date pad-with port)
		 (display (date->string date "~k:~M:~S~z") port)))
     (cons #\3 (lambda (date pad-with port)
		 (display (date->string date "~k:~M:~S") port)))
     (cons #\4 (lambda (date pad-with port)
		 (display (date->string date "~Y-~m-~dT~k:~M:~S~z") port)))
     (cons #\5 (lambda (date pad-with port)
		 (display (date->string date "~Y-~m-~dT~k:~M:~S") port)))
     ))

  (define (%padding n pad-with length)
    ;;Returns a string rep. of number  N, of minimum LENGTH, padded with
    ;;character PAD-WITH.  If PAD-WITH  if #f, no  padding is  done, and
    ;;it's  as if  number->string was  used.  if  string is  longer than
    ;;LENGTH, it's as if number->string was used.
    ;;
    (let* ((str     (number->string n))
	   (str-len (string-length str)))
      (if (or (> str-len length)
	      (not pad-with))
	  str
	(let* ((new-str        (make-string length pad-with))
	       (new-str-offset (- (string-length new-str) str-len)))
	  (do ((i 0 (+ i 1)))
	      ((>= i (string-length str)))
            (string-set! new-str (+ new-str-offset i) (string-ref str i)))
	  new-str))))

  (define (%fractional-part r)
    (if (integer? r)
	"0"
      (let* ((str  (number->string (exact->inexact r)))
	     (ppos (%char-pos #\. str 0 (string-length str))))
	(substring str (+ ppos 1) (string-length str)))))

  (define (%char-pos char str index len)
    (cond ((>= index len)
	   #f)
	  ((char=? (string-ref str index) char)
	   index)
	  (else
	   (%char-pos char str (+ index 1) len))))

  (define (%tz-printer offset port)
    (cond ((= offset 0)
	   (display "Z" port))
	  ((negative? offset)
	   (display "-" port))
	  (else
	   (display "+" port)))
    (unless (= offset 0)
      (let ((hours   (abs (quotient offset (* 60 60))))
	    (minutes (abs (quotient (remainder offset (* 60 60)) 60))))
	(display (%padding hours #\0 2)   port)
	(display (%padding minutes #\0 2) port))))

  (define (%locale-print-time-zone date port)
    ;;FIXME We should print something here.
    ;;
    (values))

  (define (%locale-am/pm hr)
    (if (> hr 11)
	LOCALE-PM
      LOCALE-AM))

;;; --------------------------------------------------------------------

  (define (%last-n-digits i n)
    (abs (remainder i (expt 10 n))))

  (define (%locale-abbr-weekday n)
    ($vector-ref LOCALE-ABBR-WEEKDAY-VECTOR n))

  (define (%locale-long-weekday n)
    ($vector-ref LOCALE-LONG-WEEKDAY-VECTOR n))

  (define (%locale-abbr-month n)
    ($vector-ref LOCALE-ABBR-MONTH-VECTOR n))

  (define (%locale-long-month n)
    ($vector-ref LOCALE-LONG-MONTH-VECTOR n))

;;; --------------------------------------------------------------------

  (define (%error-bad-template format-string)
    (error who "bad date format string" format-string))

  #| end of module |# )


;;;; done

)

;;; end of file
