
#| http://tools.ietf.org/html/rfc3986

   query         = *( pchar / "/" / "?" )
   pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"

   pct-encoded   = "%" HEXDIG HEXDIG

   unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
   sub-delims    = "!" / "$" / "&" / "'" / "(" / ")"
                 / "*" / "+" / "," / ";" / "="
|#



(module cgi (params param)
  (define (parse-query-string str)
    ; parse string as (key=value) separated by &s:
    (define (hex->integer c err)
      (let ([n (char->integer c)])
        (cond
          [(fx<= (char->integer #\0) n (char->integer #\9))
           (fx- n (char->integer #\0))]
          [(fx<= (char->integer #\A) n (char->integer #\F))
           (fx+ 10 (fx- n (char->integer #\A)))]
          [(fx<= (char->integer #\a) n (char->integer #\f))
           (fx+ 10 (fx- n (char->integer #\a)))]
          [else (err)])))
    (define (f str i n ac key err)
      (cond
        [(fx= i n)
         (if key
             (list (cons key (list->string (reverse ac))))
             (if (null? ac)
                 '()
                 (err)))]
        [else
         (let ([c (string-ref str i)])
           (cond
             [(char=? c #\&)
              (if key
                  (cons (cons key (list->string (reverse ac)))
                        (f str (fxadd1 i) n '() #f err))
                  (if (null? ac)
                      (f str (fxadd1 i) n '() #f err)
                      (err)))]
             [(char=? c #\=)
              (if key
                  (f str (fxadd1 i) n (cons c ac) key err)
                  (f str (fxadd1 i) n '() (list->string (reverse ac)) err))]
             [(char=? c #\%)
              (if (fx< (fx+ i 2) n)
                  (f str (fx+ i 3) 
                     (cons (integer->char
                             (let ([n0 (hex->integer
                                         (string-ref str (fx+ i 1))
                                         err)]
                                   [n1 (hex->integer
                                         (string-ref str (fx+ i 2))
                                         err)])
                               (fxlogor (fxsll n0 8) n1)))
                           ac)
                     n key err)
                  (err))]
             [(char=? c #\+)
              (f str (fxadd1 i) n (cons #\space ac) key err)]
             [(or (char<=? #\a c #\z)
                  (char<=? #\A c #\Z)
                  (char<=? #\0 c #\9)
                  (memq c '(#\- #\. #\_ #\~ #\! #\$ #\& #\' 
                            #\( #\) #\* #\, #\; #\= #\: #\@)))
              (f str (fx+ i 1) n (cons c ac) key err)]
             [else (err)]))]))
    (f str 0 (string-length str) '() #f
       (lambda () 
         (error 'parse-query-string "invalid query string ~s" str))))
  (define (params)
    (map car cgi-env))
  (define (param x)
    (cond
      [(assoc x cgi-env) => cdr]
      [else #f]))
  (define cgi-env
     (parse-query-string (env "QUERY_STRING")))
  #|cgi-module|#)

