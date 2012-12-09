;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for log function
;;;Date: Mon May 31, 2010
;;;
;;;Abstract
;;;
;;;	Some  test  cases  come  from  the R6RS  document;  others  from
;;;	Larceny's test suite; others from MPFR's test suite; others from
;;;	the MPC's test suite.
;;;
;;;Copyright (c) 2010, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


(import (vicare)
  (rnrs eval)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing log\n")

(define (real-quasi=? ra rb)
  (or (and (infinite? ra)
	   (infinite? rb))
      (< (abs (- ra rb)) 1e-6)))

(define (quasi=? a b)
  (let ((ra (real-part a)) (rb (real-part b))
	(ia (imag-part a)) (ib (imag-part b)))
    ;; (write (list 'xx a b))(newline)
    ;; (write (list 're ra rb (real-quasi=? ra rb)))(newline)
    ;; (write (list 'im ia ib (real-quasi=? ia ib)))(newline)
    ;; (newline)
    (or (and (nan? a)
	     (nan? b))
	(and (real-quasi=? ra rb)
	     (real-quasi=? ia ib)))))

(define R make-rectangular)
(define (B m e) (* m (expt 2 e)))


;;;; tests for fixnums

(check
    (guard (E ((assertion-violation? E)
	       (condition-message E))
	      (else #t))
      (eval '(log 0) (environment '(rnrs))))
  => "undefined around 0")

(check (log 1)		=> 0)
(check (log 10)		(=> quasi=?) 2.30258509299)
(check (log -10)	(=> quasi=?) 2.30258509299+3.14159265358793i)

(check
    (guard (E (else #t))
      (eval '(log 0) (environment '(rnrs))))
  => #t)


;;;; tests for flonums

(check (log  0.0)			(=> quasi=?) -inf.0)
(check (log  1.0)			(=> quasi=?)  0.0)
(check (log -1.0)			(=> quasi=?)  0.0+3.141592653589793i)
(check (log  10.0)			(=> quasi=?)  2.30258509299)
(check (log -10.0)			(=> quasi=?)  2.30258509299+3.14159265358793i)
(check (log -1.0+0.0i)			(=> quasi=?)  0.0+3.14159265358793i)
(check (log -1.0-0.0i)			(=> quasi=?)  0.0-3.14159265358793i)


(check (log  2.718281828459045)		(=> quasi=?)  1.0)
(check (log  0.36787944117144233)	(=> quasi=?) -1.0)
(check (log  2.0)			(=> quasi=?)  0.6931471805599453)
(check (log 10.0)			(=> quasi=?)  2.302585092994046)
(check (log  0.75)			(=> quasi=?) -0.2876820724517809)
(check (* 2.0 (log -1.0))		(=> quasi=?) 0.0+6.283185307179586i)
(check (log 1024 2)			(=> quasi=?) 10.0)
(check (log 2048.0 2.0)			(=> quasi=?) 11.0)


(check (log +inf.0)		(=> quasi=?) +inf.0)
(check (log -inf.0)		(=> quasi=?) +inf.0+3.14159265358793i)
(check (log -inf.0+1i)		(=> quasi=?) +inf.0+3.14159265358793i)

(check (log +nan.0)		(=> quasi=?) +nan.0)
(check (log +nan.0+10.0i)	(=> quasi=?) +nan.0)
(check (log +nan.0-10.0i)	(=> quasi=?) +nan.0)
(check (log 10.0+nan.0i)	(=> quasi=?) +nan.0)
(check (log 10.0+nan.0i)	(=> quasi=?) +nan.0)

;;; these come from MPFR's test suite
(check (log 1.00089971802309629645)	(=> quasi=?) 8.99313519443722736088e-04)
(check (log 1.01979300812244555452)	(=> quasi=?) 1.95996734891603630047e-02)
(check (log 1.02900871924604464525)	(=> quasi=?) 2.85959303301472726744e-02)
(check (log 1.27832870030418943585)	(=> quasi=?) 2.45553521871417795852e-01)
(check (log 1.31706530746788241792)	(=> quasi=?) 2.75406009586277422674e-01)
(check (log 1.47116981099449883885)	(=> quasi=?) 3.86057874110010412760e-01)
(check (log 1.58405446812987782401)	(=> quasi=?) 4.59987679246663727639e-01)
(check (log 1.67192331263391547047)	(=> quasi=?) 5.13974647961076613889e-01)
(check (log 1.71101198068990645318)	(=> quasi=?) 5.37084997042120315669e-01)
(check (log 1.72634853551388700588)	(=> quasi=?) 5.46008504786553605648e-01)
(check (log 2.00028876593004323325)	(=> quasi=?) 6.93291553102749702475e-01)
(check (log 6.27593230200363105808)	(=> quasi=?) 1.83672204800630312072)
(check (log 7.47216682321367997588)	(=> quasi=?) 2.01118502712453661729)
(check (log 9.34589857718275318632)	(=> quasi=?) 2.23493759221664944903)
(check (log 10.6856587560831854944)	(=> quasi=?) 2.36890253928838445674)
(check (log 12.4646345033981766903)	(=> quasi=?) 2.52289539471636015122)
(check (log 17.0953275851761752335)	(=> quasi=?) 2.83880518553861849185)
(check (log 19.8509496207496916043)	(=> quasi=?) 2.98825184582516722998)
(check (log 23.9512076062771335216)	(=> quasi=?) 3.17601874455977206679)
(check (log 428.315247165198229595)	(=> quasi=?) 6.05985948325268264369)

;;; these come from MPFR's test suite
(check (log 3)				(=> quasi=?) 1.09861228866810956)
(check (log 4947981/32768)		(=> quasi=?) 5.017282)
(check (log 1.01979300812244555452)	(=> quasi=?) 1.95996734891603664741e-02)
(check (log 10.0)			(=> quasi=?) 2.30258509299404590110e+00)
(check (log 6.0)			(=> quasi=?) 1.79175946922805517936)
(check (log 62.0)			(=> quasi=?) 4.12713438504509166905)
(check (log 0.5)			(=> quasi=?) -6.93147180559945286226e-01)
(check (log 3.0)			(=> quasi=?) 1.09861228866810956006e+00)
(check (log 234375765.0)		(=> quasi=?) 1.92724362186836231104e+01)
(check (log 8.0)			(=> quasi=?) 2.07944154167983574765e+00)
(check (log 44.0)			(=> quasi=?) 3.78418963391826146392e+00)
(check (log 1.01979300812244555452)	(=> quasi=?) 1.95996734891603664741e-02)

;;; these come from MPFR's test suite
(check (log 0.99999599881598921769)	(=> quasi=?) -0.0000040011920155404072924737977900999652547398000024259090423583984375)
(check (log 9.99995576063808955247e-01)	(=> quasi=?) -4.42394597667932383816e-06)
(check (log 9.99993687357856209097e-01) (=> quasi=?) -6.31266206860017342601e-06)
(check (log 9.99995223520736886691e-01) (=> quasi=?) -4.77649067052670982220e-06)
(check (log 9.99993025794720935551e-01) (=> quasi=?) -6.97422959894716163837e-06)
(check (log 9.99987549017837484833e-01) (=> quasi=?) -1.24510596766369924330e-05)
(check (log 9.99985901426543311032e-01) (=> quasi=?) -1.40986728425098585229e-05)
(check (log 9.99986053947420794330e-01) (=> quasi=?) -0.000013946149826301084938555592540598837558718514628708362579345703125)
(check (log 9.99971938247442126979e-01) (=> quasi=?) -2.80621462962173414790e-05)

;;; these come from MPFR's test suite
(check (log 1.18615436389927785905e+77) (=> quasi=?) 1.77469768607706015473e+02)
(check (log 9.48868723578399476187e+77) (=> quasi=?) 1.79549152432275803903e+02)
(check (log 2.31822210096938820854e+89) (=> quasi=?) 2.05770873832573869322e+02)

;;; these come from MPFR's test suite
(check (log 9.99999989485669482647e-01) (=> quasi=?) -1.05143305726283042331e-08)
(check (log 9.99999989237970177136e-01) (=> quasi=?) -1.07620298807745377934e-08)
(check (log 9.99999989239339082125e-01) (=> quasi=?) -1.07606609757704445430e-08)

;;; these come from MPFR's test suite
(check (log 7.3890560989306504)		(=> quasi=?) 2.0000000000000004) ;; exp(2.0)
(check (log 7.3890560989306495)		(=> quasi=?) 2.0) ;; exp(2.0)
(check (log 7.53428236571286402512e+34) (=> quasi=?) 8.03073567492226345621e+01)
(check (log 6.18784121531737948160e+19) (=> quasi=?) 4.55717030391710693493e+01)
(check (log 1.02560267603047283735e+00) (=> quasi=?) 2.52804164149448735987e-02)
(check (log 7.53428236571286402512e+34) (=> quasi=?) 8.03073567492226345621e+01)
(check (log 1.42470900831881198052e+49) (=> quasi=?) 113.180637144887668910087086260318756103515625)

;;; these come from MPFR's test suite
(check (log 1.08013816255293777466e+11) (=> quasi=?) 2.54055249841782604392e+01)
(check (log 6.72783635300509015581e-37) (=> quasi=?) -8.32893948416799503320e+01)
(check (log 2.25904918906057891180e-52) (=> quasi=?) -1.18919480823735682406e+02)
(check (log 1.48901209246462951085e+00) (=> quasi=?) 3.98112874867437460668e-01)
(check (log 1.70322470467612341327e-01) (=> quasi=?) -1.77006175364294615626)
(check (log 1.94572026316065240791e+01) (=> quasi=?) 2.96821731676437838842)
(check (log 4.01419512207026418764e+04) (=> quasi=?) 1.06001772315501128218e+01)
(check (log 9.47077365236487591672e-04) (=> quasi=?) -6.96212977303956748187e+00)
(check (log 3.95906157687589643802e-109) (=> quasi=?) -2.49605768114704119399e+02)
(check (log 2.73874914516503004113e-02) (=> quasi=?) -3.59766888618655977794e+00)
(check (log 9.18989072589566467669e-17) (=> quasi=?) -3.69258425351464083519e+01)
(check (log 7706036453608191045959753324430048151991964994788917248.0) (=> quasi=?) 126.3815989984199177342816255986690521240234375)
(check (log 1.74827399630587801934e-23) (=> quasi=?) -5.24008281254547156891e+01)
(check (log 4.35302958401482307665e+22) (=> quasi=?) 5.21277441046519527390e+01)
(check (log 9.70791868689332915209e+00) (=> quasi=?) 2.27294191194272210410e+00)
(check (log 2.22183639799464011100e-01) (=> quasi=?) -1.50425103275253957413e+00)
(check (log 2.27313466156682375540e+00) (=> quasi=?) 8.21159787095675608448e-01)
(check (log 6.58057413965851156767e-01) (=> quasi=?) -4.18463096196088235600e-01)
(check (log 7.34302197248998461006e+43) (=> quasi=?) 101.0049094695131799426235374994575977325439453125)
(check (log 6.09969788341579732815e+00) (=> quasi=?) 1.80823924264386204363e+00)

(check (log (B #x1 1024))	(=> quasi=?) (B #x162E42FEFA39F -39))
(check (log (B #x1 2048)) 	(=> quasi=?) (B #x162E42FEFA39F -38))
(check (log (B #x1 4096))	(=> quasi=?) (B #x162E42FEFA39F -37))
(check (log (B #x1 8192))	(=> quasi=?) (B #x162E42FEFA39F -36))
(check (log (B #x1 16384))	(=> quasi=?) (B #x162E42FEFA39F -35))


;;;; tests for complex

;;; these come from MPC's test suite
(check (log #x11)			(=> quasi=?) (B #xB5535E0FD3FBD -50))
(check (log #x11i)			(=> quasi=?) (R (B #xB5535E0FD3FBD -50)
							(B #x3243F6A8885A3 -49)))
(check (log #x-13)			(=> quasi=?) (R (B #x5E38D81812CCB  -49)
							(B #x3243F6A8885A3  -48)))
(check (log (R #x-13 -0.))		(=> quasi=?) (R (B #x5E38D81812CCB  -49)
							(B #x-3243F6A8885A3 -48)))
(check (log (R 0. #x-13))		(=> quasi=?) (R (B #x5E38D81812CCB -49)
							(B #x-3243F6A8885A3 -49)))

(check (log (R #x-17 +0.))		(=> quasi=?) (R (B #x19157DFDD1B3F -47)
							(B #x1921FB54442D19 -51)))
(check (log (R #x-17 -0.))		(=> quasi=?) (R (B #x19157DFDD1B3F -47)
							(B #x-1921FB54442D19 -51)))
(check (log (R #x-17 +0.))		(=> quasi=?) (R (B #x19157DFDD1B3F1 -51)
							(B #x3243F6A8885A3 -48)))
(check (log (R #x-17 -0.))		(=> quasi=?) (R (B #x19157DFDD1B3F -47)
							(B #x-3243F6A8885A3 -48)))
(check (log (R #x-42 +0.))		(=> quasi=?) (R (B #x8611A6D2511D3 -49)
							(B #x3243F6A8885A3 -48)))
(check (log (R #x-42 -0.))		(=> quasi=?) (R (B #x8611A6D2511D3 -49)
							(B #x-3243F6A8885A3 -48)))
(check (log (R #x-42 +0.))		(=> quasi=?) (R (B #x10C234DA4A23A7 -50)
							(B #x3243F6A8885A3 -48)))
(check (log (R #x-42 -0.))		(=> quasi=?) (inexact (R (B #x8611A6D2511D3 -49)
								 (B #x-3243F6A8885A3 -48))))

;;These fail because:
;;
;;  (inexact (B #x1 1024)) => +inf.0
;;
;;and MAKE-RECTANGULAR applied INEXACT when one operand is exact.
;;
(check (log (R (B #x1 1024)    +0.))	(=> quasi=?) (R (B #x162E42FEFA39F -39) 0.))
(check (log (R (B #x1 2048)    +0.))	(=> quasi=?) (R (B #x162E42FEFA39F -38) 0.))
(check (log (R (B #x1 4096)    +0.))	(=> quasi=?) (R (B #x162E42FEFA39F -37) 0.))
(check (log (R (B #x1 8192)    +0.))	(=> quasi=?) (R (B #x162E42FEFA39F -36) 0.))
(check (log (R (B #x1 16384)   +0.))	(=> quasi=?) (R (B #x162E42FEFA39F -35) 0.))

(check (log (R (B #x1 1024) (B #x1 1024)))	(=> quasi=?) (R (B #x163108C75A1937 -43)
								(B #x1921FB54442D19 -53)))

(check (log (R (B #x1 2048) (B #x1 2048)))	(=> quasi=?) (R (B #x162FA5E32A2993 -42)
								(B #x1921FB54442D19 -53)))

(check (log (R (B #x1 4096) (B #x1 4096)))	(=> quasi=?) (R (B #xB177A388918E1 -40)
								(B #x1921FB54442D19 -53)))

(check (log (R (B #x1 8192) (B #x1 8192)))	(=> quasi=?) (R (B #x162E9BB80635D9 -40)
								(B #x1921FB54442D19 -53)))

(check (log (R (B #x1 16384) (B #x1 16384)))	(=> quasi=?) (R (B #x58B9BD6E00DF9 -37)
								(B #x1921FB54442D19 -53)))

(check (log (R (B #x1 32768) (B #x1 32768)))	(=> quasi=?) (R (B #xB172C969E9C75 -37)
								(B #x1921FB54442D19 -53)))

(check (log (R (B #x1 65536) (B #x1 65536)))	(=> quasi=?) (R (B #x162E4E161BB96D -37)
								(B #x1921FB54442D19 -53)))

(check (log (R (B #x1 131072) (B #x1 131072)))	(=> quasi=?) (inexact (R (B #xB172445457CD7 -35)
									 0.7853981633974483)))


;;;; done

(check-report)

;;; end of file
