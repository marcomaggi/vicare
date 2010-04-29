(library (tests hashtables)
  (export run-tests)
  (import 
    (ikarus)
    (rnrs hashtables)
    (tests framework))

  (define (run-tests) (test-hashtables))

  (define-tests test-hashtables
    [values
     (let ([h (make-eq-hashtable)])
       (hashtable-set! h 'foo 12)
       (hashtable-set! h 'bar 13)
       (or (equal? (hashtable-keys h) '#(foo bar))
           (equal? (hashtable-keys h) '#(bar foo))))]
    [values
     (let ([h (make-eq-hashtable)])
       (hashtable-set! h 'foo 12)
       (hashtable-set! h 'bar 13)
       (hashtable-clear! h)
       (equal? (hashtable-keys h) '#()))]
    ))

