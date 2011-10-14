;;; do optimize fx< fx<= fx= fx> fx>=
;;; do optimize fxzero?
;;; do optimize fxadd1 fxsub1 fx+ fx- 
(module (s-car s-cdr (s-cons make-stream) stream? s-head)
  (import scheme)
  (define-record stream (car cdr))
  (define (s-car x)
    (if (stream? x)
        ((stream-car x))
        (error 's-car "~s is not a stream" x)))
  (define (s-cdr x)
    (if (stream? x)
        ((stream-cdr x))
        (error 's-cdr "~s is not a stream" x)))
  (define-syntax s-cons
    (syntax-rules ()
      [(_ a d) (make-stream (lambda () a) (lambda () d))]))
  (define (s-head n s)
    (unless (and (fixnum? n) (fx>= n 0))
      (error 's-head 
             "length must be a non-negative fixnum, got ~s"
             n))
    (unless (stream? s)
      (error 's-head "~s is not a stream" s))
    (let f ([n n] [s s] [ac '()])
      (cond
        [(fxzero? n) (reverse ac)]
        [else 
         (f (fx- n 1) (s-cdr s) (cons (s-car s) ac))]))))


  (define (going-up x lim)
    (cond ((= x lim) (s-cons x (going-down (- x 1) (+ lim 1))))
          (else (s-cons x (going-up (+ x 1) lim)))))
  (define (going-down x lim)
    (cond ((= x 0) (going-up 1 lim))
          (else (s-cons x (going-down (- x 1) lim)))))
  (define s (going-up 1 1))

#!eof

Path: g2news2.google.com!news3.google.com!newshub.sdsu.edu!tethys.csu.net!okeanos.csu.net!53ab2750!not-for-mail
Sender: luv...@localhost.localdomain
Newsgroups: comp.lang.scheme
Subject: Re: Stream problem
References: <1164352610.327618.225810@l39g2000cwd.googlegroups.com>
From: Andru Luvisi <luv...@andru.sonoma.edu>
Message-ID: <8764cq9fhg.fsf@localhost.localdomain>
Lines: 21
User-Agent: Gnus/5.09 (Gnus v5.9.0) Emacs/21.4
MIME-Version: 1.0
Content-Type: text/plain; charset=us-ascii
Date: 05 Dec 2006 13:53:31 -0800
NNTP-Posting-Host: 130.157.65.249
X-Trace: okeanos.csu.net 1165355611 130.157.65.249 (Tue, 05 Dec 2006 13:53:31 PST)
NNTP-Posting-Date: Tue, 05 Dec 2006 13:53:31 PST


Several of the answers that have been posted involve using reverse,
which has the disadvantage that the later you go in the stream, the
more of it you will need to hold in ram if you are processing and
discarding elements as you go.

  (define (going-up x lim)
    (cond ((= x lim) (s-cons x (going-down (- x 1) (+ lim 1))))
          (else (s-cons x (going-up (+ x 1) lim)))))
  (define (going-down x lim)
    (cond ((= x 0) (going-up 1 lim))
          (else (s-cons x (going-down (- x 1) lim)))))
  (define s (going-up 1 1))

Andru
-- 
Andru Luvisi

Quote Of The Moment:
  Quidquid latine dictum sit, altum viditur.
  ( Whatever is said in Latin sounds profound. )
