(library (tests strings)
  (export run-tests)
  (import (ikarus) (tests framework))

  (define (run-tests) (test-strings))

  (define-tests test-strings
    [values
     (string-ci=? "Strasse" "Stra\xDF;e")]
    ;[(lambda (x) (string=? x "STRASSE"))
    ; (string-upcase "Stra\xDF;e")]
    ;[(lambda (x) (string=? x "stra\xDF;e"))
    ; (string-downcase "Stra\xDF;e")]
    [(lambda (x) (string=? x "strasse")) 
     (string-foldcase "Stra\xDF;e")]
    ;[(lambda (x) (string=? x "strasse")) 
    ; (string-downcase "STRASSE")]
    [values (string-ci=? "Stra\xDF;e" "Strasse")]
    [values (string-ci=? "Stra\xDF;e" "STRASSE")]
    [values (string-ci=? "\xDF;" "SS")]
    [values (string-ci=? "\xDF;\xDF;" "SSSS")]
    ))


