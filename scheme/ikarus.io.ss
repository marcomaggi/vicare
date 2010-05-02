;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;; Modified by Marco Maggi
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus.io)

  (export
    port? input-port? output-port? textual-port? binary-port?
    open-file-input-port open-input-file
    call-with-input-file with-input-from-file
    standard-input-port current-input-port
    open-bytevector-input-port
    open-string-input-port open-string-input-port/id
    with-input-from-string
    make-custom-binary-input-port
    make-custom-binary-output-port
    make-custom-textual-input-port
    make-custom-textual-output-port
    transcoded-port port-transcoder
    close-port port-closed? close-input-port close-output-port
    port-eof?
    get-char lookahead-char read-char peek-char
    get-string-n get-string-n! get-string-all get-line read-line
    get-u8 lookahead-u8
    get-bytevector-n get-bytevector-n!
    get-bytevector-some get-bytevector-all
    port-position port-has-port-position?
    set-port-position! port-has-set-port-position!?
    call-with-port
    flush-output-port
    put-u8 put-bytevector
    put-char write-char
    put-string
    open-bytevector-output-port
    call-with-bytevector-output-port
    open-string-output-port with-output-to-string
    with-output-to-port
    call-with-string-output-port
    open-output-string get-output-string
    standard-output-port standard-error-port
    current-output-port current-error-port
    open-file-output-port open-output-file
    call-with-output-file with-output-to-file
    console-output-port
    console-error-port
    console-input-port
    newline
    port-mode set-port-mode!
    output-port-buffer-mode
    reset-input-port!
    reset-output-port!
    port-id
    input-port-byte-position
    input-port-column-number input-port-row-number
    process process-nonblocking
    process*

    tcp-connect tcp-connect-nonblocking
    udp-connect udp-connect-nonblocking
    tcp-server-socket tcp-server-socket-nonblocking
    accept-connection accept-connection-nonblocking
    close-tcp-server-socket
    register-callback
    input-socket-buffer-size output-socket-buffer-size

    open-directory-stream directory-stream?
    read-directory-stream close-directory-stream)



  (import
    (ikarus system $io)
    (except (ikarus)
      port? input-port? output-port? textual-port? binary-port?
      open-file-input-port open-input-file
      call-with-input-file with-input-from-file
      standard-input-port current-input-port
      open-bytevector-input-port
      open-string-input-port with-input-from-string
      make-custom-binary-input-port
      make-custom-binary-output-port
      make-custom-textual-input-port
      make-custom-textual-output-port
      transcoded-port port-transcoder
      close-port port-closed? close-input-port close-output-port
      port-eof?
      get-char lookahead-char read-char peek-char
      get-string-n get-string-n! get-string-all get-line read-line
      get-u8 lookahead-u8
      get-bytevector-n get-bytevector-n!
      get-bytevector-some get-bytevector-all
      port-position port-has-port-position?
      set-port-position! port-has-set-port-position!?
      call-with-port
      flush-output-port
      put-u8 put-bytevector
      put-char write-char
      put-string
      open-bytevector-output-port
      call-with-bytevector-output-port
      open-string-output-port with-output-to-string
      call-with-string-output-port
      open-output-string get-output-string
      standard-output-port standard-error-port
      current-output-port current-error-port
      open-file-output-port open-output-file
      call-with-output-file with-output-to-file
      with-output-to-port
      console-output-port
      console-input-port
      console-error-port
      newline
      port-mode set-port-mode!
      output-port-buffer-mode
      reset-input-port!
      reset-output-port!
      port-id
      process process-nonblocking
      tcp-connect tcp-connect-nonblocking
      udp-connect udp-connect-nonblocking
      tcp-server-socket tcp-server-socket-nonblocking
      accept-connection accept-connection-nonblocking
      close-tcp-server-socket
      register-callback
      input-socket-buffer-size output-socket-buffer-size
      input-port-column-number input-port-row-number

      open-directory-stream directory-stream?
      read-directory-stream close-directory-stream
      process*
      ))

  ;(define-syntax assert* (identifier-syntax assert))

  ;;; all-data-in-buffer is used in place of the read!
  ;;; procedure to mark ports whose buffer is all the
  ;;; data there is.
  (define all-data-in-buffer 'all-data-in-buffer)

  (define-syntax assert* (syntax-rules () [(_ . x) (void)]))

  (module UNSAFE
    (fx< fx<= fx> fx>= fx= fx+ fx-
     fxior fxand fxsra fxsll
     integer->char char->integer
     string-ref string-set! string-length
     bytevector-u8-ref bytevector-u8-set!
     bytevector-u16-ref)
    (import
      (rename (ikarus system $strings)
        ($string-length string-length)
        ($string-ref    string-ref)
        ($string-set!   string-set!))
      (rename (ikarus system $chars)
        ($char->fixnum char->integer)
        ($fixnum->char integer->char))
      (rename (ikarus system $bytevectors)
        ($bytevector-set!   bytevector-u8-set!)
        ($bytevector-u8-ref bytevector-u8-ref))
      (rename (ikarus system $fx)
        ($fxsra    fxsra)
        ($fxsll    fxsll)
        ($fxlogor  fxior)
        ($fxlogand fxand)
        ($fx+      fx+)
        ($fx-      fx-)
        ($fx<      fx<)
        ($fx>      fx>)
        ($fx>=     fx>=)
        ($fx<=     fx<=)
        ($fx=      fx=)))
    (define (bytevector-u16-ref x i endianness)
      (case endianness
        [(little)
         (fxlogor (bytevector-u8-ref x i)
                  (fxsll (bytevector-u8-ref x (fx+ i 1)) 8))]
        [else
         (fxlogor (bytevector-u8-ref x (fx+ i 1))
                  (fxsll (bytevector-u8-ref x i) 8))])))


  (define (port? x)
    (import (only (ikarus) port?))
    (port? x))

  (define-syntax define-rrr
    (syntax-rules ()
      [(_ name)
       (define (name . args)
         (apply die 'name "not implemented" args))]))

  (define-syntax u8?
    (let ()
      (import (ikarus system $fx))
      (syntax-rules ()
        [(_ x)
         ($fxzero? ($fxlogand x -256))])))

  ;(define (u8? x) (and (fixnum? x) (fx>= x 0) (fx< x 256)))

  (define (textual-port? x)
    (fx= (fxand ($port-tag x) textual-port-tag) textual-port-tag))

  (define (binary-port? x)
    (fx= (fxand ($port-tag x) binary-port-tag) binary-port-tag))

  (define (output-port? x)
    (fx= (fxand ($port-tag x) output-port-tag) output-port-tag))

  (define (input-port? x)
    (fx= (fxand ($port-tag x) input-port-tag) input-port-tag))

  ;;; everything above this line will turn into primitive
  ;;; ----------------------------------------------------------

  (define input-port-tag           #b00000000000001)
  (define output-port-tag          #b00000000000010)
  (define textual-port-tag         #b00000000000100)
  (define binary-port-tag          #b00000000001000)
  (define fast-char-text-tag       #b00000000010000)
  (define fast-u7-text-tag         #b00000000100000)
  (define fast-u8-text-tag         #b00000001100000)
  (define fast-u16be-text-tag      #b00000010000000)
  (define fast-u16le-text-tag      #b00000100000000)
  (define init-u16-text-tag        #b00000110000000)
;  (define r6rs-mode-tag            #b01000000000000)
  (define closed-port-tag          #b10000000000000)

  (define port-type-mask           #b00000000001111)
  (define binary-input-port-bits   #b00000000001001)
  (define binary-output-port-bits  #b00000000001010)
  (define textual-input-port-bits  #b00000000000101)
  (define textual-output-port-bits #b00000000000110)

  (define fast-get-byte-tag        #b00000000001001)
  (define fast-get-char-tag        #b00000000010101)
  (define fast-get-utf8-tag        #b00000000100101)
  (define fast-get-latin-tag       #b00000001100101)
  (define fast-get-utf16be-tag     #b00000010000101)
  (define fast-get-utf16le-tag     #b00000100000101)

  (define fast-put-byte-tag        #b00000000001010)
  (define fast-put-char-tag        #b00000000010110)
  (define fast-put-utf8-tag        #b00000000100110)
  (define fast-put-latin-tag       #b00000001100110)
  (define fast-put-utf16be-tag     #b00000010000110)
  (define fast-put-utf16le-tag     #b00000100000110)
  (define init-put-utf16-tag       #b00000110000110)

  (define fast-attrs-mask          #b111111111111)
  (define-syntax $port-fast-attrs
    (identifier-syntax
      (lambda (x)
        (import (ikarus system $fx))
        ($fxlogand ($port-tag x) fast-attrs-mask))))

  (define-struct cookie (dest mode pos row-num newline-pos))

  (define (default-cookie fd) (make-cookie fd 'vicare-mode 0 0 0))

  (define (port-id p)
    (if (port? p)
        ($port-id p)
        (die 'port-id "not a port" p)))

  (define (input-port-byte-position p)
    (if (input-port? p)
        (let ([cookie ($port-cookie p)])
           (+ (cookie-pos cookie) (fx+ ($port-index p) 1)))
        (error 'input-port-byte-position "not an input port" p)))

  (define (mark/return-newline p)
    (let ([cookie ($port-cookie p)])
      (set-cookie-row-num! cookie (+ (cookie-row-num cookie) 1))
      (set-cookie-newline-pos! cookie
        (+ (cookie-pos cookie) ($port-index p))))
    #\newline)

  (define (input-port-column-number p)
    (if (input-port? p)
        (let ([cookie ($port-cookie p)])
          (- (+ (cookie-pos cookie) ($port-index p))
             (cookie-newline-pos cookie)))
        (die 'input-port-column-number "not an input port" p)))

  (define (input-port-row-number p)
    (if (input-port? p)
        (cookie-row-num ($port-cookie p))
        (die 'input-port-row-number "not an input port" p)))

  (define (port-position p)
    (define who 'port-position)
    (if (port? p)
        (let ([cookie       ($port-cookie p)]
              [index        ($port-index p)]
              [get-position ($port-get-position p)])
          (cond
            [(procedure? get-position)
             (let ([pos (get-position)])
               (if (or (fixnum? pos) (bignum? pos))
                   (if (input-port? p)
                       (- pos (- ($port-size p) index))
                       (+ pos index))
                   (die who "invalid returned value from get-position" p)))]
            [(eqv? get-position #t)
             (+ (cookie-pos cookie) index)]
            [else
             (die who "port does not support port-position operation" p)]))
        (die who "not a port" p)))


  (define (set-port-position! p pos)
    (define who 'set-port-position!)
    (define (set-position! p pos flush?)
      (let ([setpos! ($port-set-position! p)])
        (cond
          [(procedure? setpos!)
           (when flush? (flush-output-port p))
           (setpos! pos)
           ($set-port-index! p 0)
           ($set-port-size! p 0)
           (let ([cookie ($port-cookie p)])
             (set-cookie-pos! cookie pos))]
          [(eqv? setpos! #t)
           (if (<= pos ($port-size p))
               ($set-port-index! p pos)
               (die who "position out of range" pos))]
          [else
           (die who "port does not support port position" p)])))
    (unless (and (or (fixnum? pos) (bignum? pos)) (>= pos 0))
      (die who "position must be a nonnegative exact integer" pos))
    (cond
      [(output-port? p) (set-position! p pos #t)]
      [(input-port? p)  (set-position! p pos #f)]
      [else (die who "not a port" p)]))


  (define (port-has-port-position? p)
    (define who 'port-has-port-position?)
    (if (port? p)
        (and ($port-get-position p) #t)
        (die who "not a port" p)))

  (define (port-has-set-port-position!? p)
    (define who 'port-has-set-port-position!?)
    (if (port? p)
        (and ($port-set-position! p) #t)
        (die who "not a port" p)))

  (define guarded-port
    (let ([G (make-guardian)])
      (define (clean-up)
        (cond
          [(G) =>
           (lambda (p)
             (close-port p)
             (clean-up))]))
      (lambda (p)
        (clean-up)
        (when (fixnum? (cookie-dest ($port-cookie p)))
          (G p))
        p)))

  (define ($make-custom-binary-port attrs init-size id
            read! write! get-position set-position! close buffer-size)
    (let ([bv (make-bytevector buffer-size)])
      ($make-port attrs 0 init-size bv #f id read! write!
                  get-position set-position! close
                  (default-cookie #f))))

  (define ($make-custom-textual-port attrs init-size id
            read! write! get-position set-position! close buffer-size)
    (let ([bv (make-string buffer-size)])
      ($make-port attrs 0 init-size bv #t id read! write!
                  get-position set-position! close
                  (default-cookie #f))))

  (define (make-custom-binary-input-port id
            read! get-position set-position! close)
    ;;; FIXME: get-position and set-position! are ignored for now
    (define who 'make-custom-binary-input-port)
    (unless (string? id)
      (die who "id is not a string" id))
    (unless (procedure? read!)
      (die who "read! is not a procedure" read!))
    (unless (or (procedure? close) (not close))
      (die who "close should be either a procedure or #f" close))
    (unless (or (procedure? get-position)
                (not get-position))
      (die who "get-position is not a procedure or #f"
           get-position))
    ($make-custom-binary-port
      binary-input-port-bits
      0
      id read! #f
      get-position
      set-position!
      close 256))

  (define (make-custom-binary-output-port id
            write! get-position set-position! close)
    ;;; FIXME: get-position and set-position! are ignored for now
    (define who 'make-custom-binary-output-port)
    (unless (string? id)
      (die who "id is not a string" id))
    (unless (procedure? write!)
      (die who "write! is not a procedure" write!))
    (unless (or (procedure? close) (not close))
      (die who "close should be either a procedure or #f" close))
    (unless (or (procedure? get-position)
                (not get-position))
      (die who "get-position is not a procedure or #f"
           get-position))
    ($make-custom-binary-port
      binary-output-port-bits
      256
      id #f write!
      get-position
      set-position!
      close 256))

  (define (make-custom-textual-input-port id
            read! get-position set-position! close)
    ;;; FIXME: get-position and set-position! are ignored for now
    (define who 'make-custom-textual-input-port)
    (unless (string? id)
      (die who "id is not a string" id))
    (unless (procedure? read!)
      (die who "read! is not a procedure" read!))
    (unless (or (procedure? close) (not close))
      (die who "close should be either a procedure or #f" close))
    (unless (or (procedure? get-position)
                (not get-position))
      (die who "get-position is not a procedure or #f"
           get-position))
    ($make-custom-textual-port
      (fxior textual-input-port-bits fast-char-text-tag)
      0
      id read! #f get-position
      set-position! close 256))

  (define (make-custom-textual-output-port id
            write! get-position set-position! close)
    ;;; FIXME: get-position and set-position! are ignored for now
    (define who 'make-custom-textual-output-port)
    (unless (string? id)
      (die who "id is not a string" id))
    (unless (procedure? write!)
      (die who "write! is not a procedure" write!))
    (unless (or (procedure? close) (not close))
      (die who "close should be either a procedure or #f" close))
    (unless (or (procedure? get-position)
                (not get-position))
      (die who "get-position is not a procedure or #f"
           get-position))
    ($make-custom-textual-port
      (fxior textual-output-port-bits fast-char-text-tag)
      256
      id #f write! get-position
      set-position! close 256))



  (define (input-transcoder-attrs x who)
    (cond
      [(not x) ;;; binary input port
       binary-input-port-bits]
      [(not (eq? 'none (transcoder-eol-style x)))
       (die who "unsupported transcoder eol-style"
            (transcoder-eol-style x))]
      [(eq? 'latin-1-codec (transcoder-codec x))
       (fxior textual-input-port-bits fast-u8-text-tag)]
      ;;; attrs for utf-8-codec are set as part of the
      ;;; bom-reading dance when the first char is read.
      [else textual-input-port-bits]))

  (define (output-transcoder-attrs x who)
    (cond
      [(not x) ;;; binary input port
       binary-output-port-bits]
      [(not (eq? 'none (transcoder-eol-style x)))
       (die who "unsupported transcoder eol-style"
            (transcoder-eol-style x))]
      [(eq? 'latin-1-codec (transcoder-codec x))
       (fxior textual-output-port-bits fast-u8-text-tag)]
      [(eq? 'utf-8-codec (transcoder-codec x))
       (fxior textual-output-port-bits fast-u7-text-tag)]
      [(eq? 'utf-16-codec (transcoder-codec x))
       (fxior textual-output-port-bits fast-u16be-text-tag)]
      [else (die who "unsupported codec" (transcoder-codec x))]))

  (define open-bytevector-input-port
    (case-lambda
      [(bv) (open-bytevector-input-port bv #f)]
      [(bv maybe-transcoder)
       (unless (bytevector? bv)
         (die 'open-bytevector-input-port
                "not a bytevector" bv))
       (when (and maybe-transcoder
                  (not (transcoder? maybe-transcoder)))
         (die 'open-bytevector-input-port
                "not a transcoder" maybe-transcoder))
       ($make-port
          (input-transcoder-attrs maybe-transcoder
            'open-bytevector-output-port)
          0 (bytevector-length bv) bv
          maybe-transcoder
          "*bytevector-input-port*"
          all-data-in-buffer ;;; read!
          #f ;;; write!
          #t ;;; get-position
          #t ;;; set-position!
          #f ;;; close
          (default-cookie #f))]))

  (define open-bytevector-output-port
    (case-lambda
      [() (open-bytevector-output-port #f)]
      [(transcoder)
       (define who 'open-bytevector-output-port)
       (unless (or (not transcoder) (transcoder? transcoder))
         (die who "invalid transcoder value" transcoder))
       (let ([buf* '()] [buffer-size 256])
         (let ([p
                ($make-port
                   (output-transcoder-attrs transcoder
                     'open-bytevector-output-port)
                   0 buffer-size (make-bytevector buffer-size)
                   transcoder
                   "*bytevector-output-port*"
                   #f
                   (lambda (bv i c)
                     (unless (= c 0)
                       (let ([x (make-bytevector c)])
                         (bytevector-copy! bv i x 0 c)
                         (set! buf* (cons x buf*))))
                     c)
                   #t ;;; get-position
                   #f ;;; set-position!
                   #f ;;; close
                   (default-cookie #f))])
           (values
             p
             (lambda ()
               (define (append-bv-buf* ls)
                 (let f ([ls ls] [i 0])
                   (cond
                     [(null? ls)
                      (values (make-bytevector i) 0)]
                     [else
                      (let* ([a (car ls)]
                             [n (bytevector-length a)])
                        (let-values ([(bv i) (f (cdr ls) (fx+ i n))])
                          (bytevector-copy! a 0 bv i n)
                          (values bv (fx+ i n))))])))
               (unless ($port-closed? p)
                 (flush-output-port p))
               (let-values ([(bv len) (append-bv-buf* buf*)])
                 (set! buf* '())
                 bv)))))]))

  (define call-with-bytevector-output-port
    (case-lambda
      [(proc) (call-with-bytevector-output-port proc #f)]
      [(proc transcoder)
       (define who 'call-with-bytevector-output-port)
       (unless (procedure? proc)
         (die who "not a procedure" proc))
       (unless (or (not transcoder) (transcoder? transcoder))
         (die who "invalid transcoder argument" transcoder))
       (let-values ([(p extract)
                     (open-bytevector-output-port transcoder)])
         (proc p)
         (extract))]))

  (define (call-with-string-output-port proc)
    (define who 'call-with-string-output-port)
    (unless (procedure? proc)
      (die who "not a procedure" proc))
    (let-values ([(p extract) (open-string-output-port)])
      (proc p)
      (extract)))

  (define (with-output-to-string proc)
    (define who 'with-output-to-string)
    (unless (procedure? proc)
      (die who "not a procedure" proc))
    (let-values ([(p extract) (open-string-output-port)])
      (parameterize ([current-output-port p])
        (proc))
      (extract)))

  (define (with-output-to-port p proc)
    (define who 'with-output-to-port)
    (unless (procedure? proc)
      (die who "not a procedure" proc))
    (unless (output-port? p)
      (die who "not an output port" p))
    (unless (textual-port? p)
      (die who "not a textual port" p))
    (parameterize ([current-output-port p])
      (proc)))

  (define (open-output-string)
    (define who 'open-output-string)
    (let ([cookie (default-cookie '())]
          [buffer-size 256])
      ($make-port
         (fxior textual-output-port-bits fast-char-text-tag)
         0 buffer-size (make-string buffer-size)
         #t ;;; transcoder
         "*string-output-port*"
         #f
         (lambda (str i c)
           (unless (= c 0)
             (let ([x (make-string c)])
               (string-copy! str i x 0 c)
               (set-cookie-dest! cookie (cons x (cookie-dest cookie)))))
           c)
         #t ;;; get-position
         #f ;;; set-position!
         #f ;;; close!
         cookie)))

  (define (open-string-output-port)
    (let ([p (open-output-string)])
      (values
        p
        (lambda ()
          (let ([str (get-output-string p)])
            (set-cookie-dest! ($port-cookie p) '())
            str)))))

  (define (get-output-string-cookie-data cookie)
    (define (append-str-buf* ls)
      (let f ([ls ls] [i 0])
        (cond
          [(null? ls)
           (values (make-string i) 0)]
          [else
           (let* ([a (car ls)]
                  [n (string-length a)])
             (let-values ([(bv i) (f (cdr ls) (fx+ i n))])
               (string-copy! a 0 bv i n)
               (values bv (fx+ i n))))])))
      (let ([buf* (cookie-dest cookie)])
        (let-values ([(bv len) (append-str-buf* buf*)])
          bv)))

  (define (get-output-string p)
    (if (port? p)
        (let ([cookie ($port-cookie p)])
          (cond
            [(and (cookie? cookie)
                  (let ([x (cookie-dest cookie)])
                    (or (null? x) (pair? x))))
             (unless ($port-closed? p)
               (flush-output-port p))
             (get-output-string-cookie-data cookie)]
            [else
             (die 'get-output-string "not an output-string port" p)]))
        (die 'get-output-string "not a port" p)))



  (define (open-string-input-port/id str id)
    (unless (string? str)
      (die 'open-string-input-port "not a string" str))
    ($make-port
       (fxior textual-input-port-bits fast-char-text-tag)
       0 (string-length str) str
       #t ;;; transcoder
       id
       all-data-in-buffer ;;; read!
       #f ;;; write!
       #t ;;; get-position
       #t ;;; set-position!
       #f ;;; close
       (default-cookie #f)))

  (define (open-string-input-port str)
    (open-string-input-port/id str "*string-input-port*"))

  (define (transcoded-port p transcoder)
    (define who 'transcoded-port)
    (unless (transcoder? transcoder)
      (die who "not a transcoder" transcoder))
    (unless (port? p) (die who "not a port" p))
    (when ($port-transcoder p) (die who "not a binary port" p))
    (when ($port-closed? p) (die who "cannot transcode closed port" p))
    (let ([read! ($port-read! p)]
          [write! ($port-write! p)])
      ($mark-port-closed! p)
      (guarded-port
        ($make-port
          (cond
            [read! (input-transcoder-attrs transcoder
                     'transcoded-port)]
            [write! (output-transcoder-attrs transcoder
                      'transcoded-port)]
            [else
             (die 'transcoded-port
               "port is neither input nor output!")])
          ($port-index p)
          ($port-size p)
          ($port-buffer p)
          transcoder
          ($port-id p)
          read!
          write!
          ($port-get-position p)
          ($port-set-position! p)
          ($port-close p)
          ($port-cookie p)))))

  (define (reset-input-port! p)
    (if (input-port? p)
        (begin
          ($set-port-index! p ($port-size p))
          (unregister-callback p))
        (die 'reset-input-port! "not an input port" p)))

  (define (reset-output-port! p)
    (if (output-port? p)
        (begin
          ($set-port-index! p 0)
          (unregister-callback p))
        (die 'reset-output-port! "not an output port" p)))


  (define (port-transcoder p)
    (if (port? p)
        (let ([tr ($port-transcoder p)])
          (and (transcoder? tr) tr))
        (die 'port-transcoder "not a port" p)))

  (define ($port-closed? p)
    (import UNSAFE)
    (not (fx= (fxand ($port-attrs p) closed-port-tag) 0)))

  (define (port-closed? p)
    (if (port? p)
        ($port-closed? p)
        (error 'port-closed? "not a port" p)))

  (define ($mark-port-closed! p)
    ($set-port-attrs! p
      (fxior closed-port-tag
             (fxand ($port-attrs p) port-type-mask))))

  (define (port-mode p)
    (if (port? p)
        (cookie-mode ($port-cookie p))
        (die 'port-mode "not a port" p)))

  (define (set-port-mode! p mode)
    (if (port? p)
        (case mode
          [(r6rs-mode vicare-mode)
           (set-cookie-mode! ($port-cookie p) mode)]
          [else (die 'set-port-mode! "invalid mode" mode)])
        (die 'set-port-mode! "not a port" p)))

  (define (put-byte/unbuffered! p b who)
    (when ($port-closed? p) (die who "port is closed" p))
    (let ([bv (make-bytevector 1)])
      (bytevector-u8-set! bv 0 b)
      (let ([bytes (($port-write! p) bv 0 1)])
        (cond
          [(eq? bytes 1)
           (let ([cookie ($port-cookie p)])
             (set-cookie-pos! cookie (+ (cookie-pos cookie) 1)))]
          [(eq? bytes 0)
           ($mark-port-closed! p)
           (die who "could not write bytes to sink")]
          [else
           (die who "invalid return value from write! proc" bytes p)]))))

  (define (put-char/unbuffered! p c who)
    (when ($port-closed? p) (die who "port is closed" p))
    (let ([str (string c)])
      (let ([bytes (($port-write! p) str 0 1)])
        (cond
          [(eq? bytes 1)
           (let ([cookie ($port-cookie p)])
             (set-cookie-pos! cookie (+ (cookie-pos cookie) 1)))]
          [(eq? bytes 0)
           ($mark-port-closed! p)
           (die who "could not write char to sink")]
          [else
           (die who "invalid return value from write! proc" bytes p)]))))

  (define flush-output-port
    (case-lambda
      [() (flush-output-port (current-output-port))]
      [(p)
       (import UNSAFE)
       (unless (output-port? p)
         (die 'flush-output-port "not an output port" p))
       (when ($port-closed? p)
         (die 'flush-output-port "port is closed" p))
       (let ([idx ($port-index p)]
             [buf ($port-buffer p)])
         (unless (fx= idx 0)
           (let ([bytes (($port-write! p) buf 0 idx)])
             (unless (and (fixnum? bytes) (fx>= bytes 0) (fx<= bytes idx))
               (die 'flush-output-port
                      "write! returned an invalid value"
                      bytes))
             (let ([cookie ($port-cookie p)])
               (set-cookie-pos! cookie (+ (cookie-pos cookie) bytes)))
             (cond
               [(fx= bytes idx)
                ($set-port-index! p 0)]
               [(fx= bytes 0)
                ($mark-port-closed! p)
                (die 'flush-output-port "could not write bytes to sink")]
               [else
                (bytevector-copy! buf bytes buf 0 (fx- idx bytes))
                ($set-port-index! p (fx- idx bytes))
                (flush-output-port p)]))))]))

  (define ($close-port p)
    (cond
      [($port-closed? p) (void)]
      [else
       (when ($port-write! p)
         (flush-output-port p))
       ($mark-port-closed! p)
       (let ([close ($port-close p)])
         (when (procedure? close)
           (close)))]))

  (define (close-port p)
    (unless (port? p)
       (die 'close-port "not a port" p))
    ($close-port p))

  (define (close-input-port p)
    (unless (input-port? p)
       (die 'close-input-port "not an input port" p))
    ($close-port p))

  (define (close-output-port p)
    (unless (output-port? p)
       (die 'close-output-port "not an output port" p))
    ($close-port p))

  (define (refill-bv-buffer p who)
    (when ($port-closed? p) (die who "port is closed" p))
    (let ([read! ($port-read! p)])
      (if (eq? read! all-data-in-buffer)
          0
          (let ([bv ($port-buffer p)] [i ($port-index p)] [j ($port-size p)])
            (let ([c0 (fx- j i)])
              (unless (fx= c0 0) (bytevector-copy! bv i bv 0 c0))
              (let ([cookie ($port-cookie p)])
                (set-cookie-pos! cookie (+ (cookie-pos cookie) i)))
              (let* ([max (fx- (bytevector-length bv) c0)]
                     [c1 (read! bv c0 max)])
                (unless (fixnum? c1)
                  (die who "invalid return value from read! procedure" c1))
                (cond
                  [(fx>= c1 0)
                   (unless (fx<= c1 max)
                     (die who "read! returned a value out of range" c1))
                   ($set-port-index! p 0)
                   ($set-port-size! p (fx+ c1 c0))
                   c1]
                  [else
                   (die who "read! returned a value out of range" c1)])))))))

  ;;; ----------------------------------------------------------
  (module (read-char get-char lookahead-char)
    (import UNSAFE)
    (define (get-char-latin-mode p who inc)
      (let ([n (refill-bv-buffer p who)])
        (cond
          [(fx= n 0) (eof-object)]
          [else
           (let ([idx ($port-index p)])
             ($set-port-index! p (fx+ idx inc))
             (integer->char (bytevector-u8-ref ($port-buffer p) idx)))])))
    (define (get-char-utf8-mode p who)
      (define (do-error p who)
        (case (transcoder-error-handling-mode ($port-transcoder p))
          [(ignore)  (get-char p)]
          [(replace) #\xFFFD]
          [(raise)
           (raise (make-i/o-decoding-error p))]
          [else (die who "cannot happen")]))
      (let ([i ($port-index p)]
            [j ($port-size p)]
            [buf ($port-buffer p)])
        (cond
          [(fx= i j) ;;; exhausted
           (let ([bytes (refill-bv-buffer p who)])
             (cond
               [(fx= bytes 0) (eof-object)]
               [else (get-char p)]))]
          [else
           (let ([b0 (bytevector-u8-ref buf i)])
             (cond
               [(fx= (fxsra b0 5) #b110) ;;; two-byte-encoding
                (let ([i (fx+ i 1)])
                  (cond
                    [(fx< i j)
                     (let ([b1 (bytevector-u8-ref buf i)])
                       (cond
                         [(fx= (fxsra b1 6) #b10)
                          ($set-port-index! p (fx+ i 1))
                          (integer->char
                            (fxior (fxand b1 #b111111)
                                   (fxsll (fxand b0 #b11111) 6)))]
                         [else
                          ($set-port-index! p i)
                          (do-error p who)]))]
                    [else
                     (let ([bytes (refill-bv-buffer p who)])
                       (cond
                         [(fx= bytes 0)
                          ($set-port-index! p (fx+ ($port-index p) 1))
                          (do-error p who)]
                         [else (get-char-utf8-mode p who)]))]))]
               [(fx= (fxsra b0 4) #b1110) ;;; three-byte-encoding
                (cond
                  [(fx< (fx+ i 2) j)
                   (let ([b1 (bytevector-u8-ref buf (fx+ i 1))]
                         [b2 (bytevector-u8-ref buf (fx+ i 2))])
                     (cond
                       [(fx= (fxsra (fxlogor b1 b2) 6) #b10)
                        (let ([n (fxlogor
                                   (fxsll (fxand b0 #b1111) 12)
                                   (fxsll (fxand b1 #b111111) 6)
                                   (fxand b2 #b111111))])
                          (cond
                            [(and (fx<= #xD800 n) (fx<= n #xDFFF))
                             ($set-port-index! p (fx+ i 1))
                             (do-error p who)]
                            [else
                             ($set-port-index! p (fx+ i 3))
                             (integer->char n)]))]
                       [else
                        ($set-port-index! p (fx+ i 1))
                        (do-error p who)]))]
                  [else
                   (let ([bytes (refill-bv-buffer p who)])
                     (cond
                       [(fx= bytes 0)
                        ($set-port-index! p (fx+ ($port-index p) 1))
                        (do-error p who)]
                       [else (get-char-utf8-mode p who)]))])]
               [(fx= (fxsra b0 3) #b11110) ;;; four-byte-encoding
                (cond
                  [(fx< (fx+ i 3) j)
                   (let ([b1 (bytevector-u8-ref buf (fx+ i 1))]
                         [b2 (bytevector-u8-ref buf (fx+ i 2))]
                         [b3 (bytevector-u8-ref buf (fx+ i 3))])
                     (cond
                       [(fx= (fxsra (fxlogor b1 b2 b3) 6) #b10)
                        (let ([n (fxlogor
                                   (fxsll (fxand b0 #b111) 18)
                                   (fxsll (fxand b1 #b111111) 12)
                                   (fxsll (fxand b2 #b111111) 6)
                                   (fxand b3 #b111111))])
                          (cond
                            [(and (fx<= #x10000 n) (fx<= n #x10FFFF))
                             ($set-port-index! p (fx+ i 4))
                             (integer->char n)]
                            [else
                             ($set-port-index! p (fx+ i 1))
                             (do-error p who)]))]
                       [else
                        ($set-port-index! p (fx+ i 1))
                        (do-error p who)]))]
                  [else
                   (let ([bytes (refill-bv-buffer p who)])
                     (cond
                       [(fx= bytes 0)
                        ($set-port-index! p (fx+ ($port-index p) 1))
                        (do-error p who)]
                       [else (get-char-utf8-mode p who)]))])]
               [else
                ($set-port-index! p (fx+ i 1))
                (do-error p who)]))])))

    (define (lookahead-char-utf8-mode p who)
      (define (do-error p who)
        (case (transcoder-error-handling-mode ($port-transcoder p))
          [(ignore) (lookahead-char p)]
          [(replace) #\xFFFD]
          [(raise)
           (raise (make-i/o-decoding-error p))]
          [else (die who "cannot happen")]))
      (let ([i ($port-index p)]
            [j ($port-size p)]
            [buf ($port-buffer p)])
        (cond
          [(fx= i j) ;;; exhausted
           (let ([bytes (refill-bv-buffer p who)])
             (cond
               [(fx= bytes 0) (eof-object)]
               [else (lookahead-char p)]))]
          [else
           (let ([b0 (bytevector-u8-ref buf i)])
             (cond
               [(fx= (fxsra b0 5) #b110) ;;; two-byte-encoding
                (let ([i (fx+ i 1)])
                  (cond
                    [(fx< i j)
                     (let ([b1 (bytevector-u8-ref buf i)])
                       (cond
                         [(fx= (fxsra b1 6) #b10)
                          (integer->char
                            (fxior (fxand b1 #b111111)
                                   (fxsll (fxand b0 #b11111) 6)))]
                         [else
                          (do-error p who)]))]
                    [else
                     (let ([bytes (refill-bv-buffer p who)])
                       (cond
                         [(fx= bytes 0) (do-error p who)]
                         [else (lookahead-char-utf8-mode p who)]))]))]
               [(fx= (fxsra b0 4) #b1110) ;;; three-byte-encoding
                (cond
                  [(fx< (fx+ i 2) j)
                   (let ([b1 (bytevector-u8-ref buf (fx+ i 1))]
                         [b2 (bytevector-u8-ref buf (fx+ i 2))])
                     (cond
                       [(fx= (fxsra (fxlogor b1 b2) 6) #b10)
                        (let ([n (fxlogor
                                   (fxsll (fxand b0 #b1111) 12)
                                   (fxsll (fxand b1 #b111111) 6)
                                   (fxand b2 #b111111))])
                          (cond
                            [(and (fx<= #xD800 n) (fx<= n #xDFFF))
                             (do-error p who)]
                            [else (integer->char n)]))]
                       [else (do-error p who)]))]
                  [else
                   (let ([bytes (refill-bv-buffer p who)])
                     (cond
                       [(fx= bytes 0) (do-error p who)]
                       [else (lookahead-char-utf8-mode p who)]))])]
               [(fx= (fxsra b0 3) #b11110) ;;; four-byte-encoding
                (cond
                  [(fx< (fx+ i 3) j)
                   (let ([b1 (bytevector-u8-ref buf (fx+ i 1))]
                         [b2 (bytevector-u8-ref buf (fx+ i 2))]
                         [b3 (bytevector-u8-ref buf (fx+ i 3))])
                     (cond
                       [(fx= (fxsra (fxlogor b1 b2 b3) 6) #b10)
                        (let ([n (fxlogor
                                   (fxsll (fxand b0 #b111) 18)
                                   (fxsll (fxand b1 #b111111) 12)
                                   (fxsll (fxand b2 #b111111) 6)
                                   (fxand b3 #b111111))])
                          (cond
                            [(and (fx<= #x10000 n) (fx<= n #x10FFFF))
                             (integer->char n)]
                            [else
                             (do-error p who)]))]
                       [else
                        (do-error p who)]))]
                  [else
                   (let ([bytes (refill-bv-buffer p who)])
                     (cond
                       [(fx= bytes 0)
                        (do-error p who)]
                       [else (lookahead-char-utf8-mode p who)]))])]
               [else (do-error p who)]))])))
    ;;;
    (define (advance-bom p who bom-seq)
      ;;; return eof if port is eof,
      ;;; #t if a bom is present, updating the port index to
      ;;;    point just past the bom.
      ;;; #f otherwise.
      (cond
        [(fx< ($port-index p) ($port-size p))
         (let f ([i 0] [ls bom-seq])
           (cond
             [(null? ls)
              ($set-port-index! p (fx+ ($port-index p) i))
              #t]
             [else
              (let ([idx (fx+ i ($port-index p))])
                (cond
                  [(fx< idx ($port-size p))
                   (if (fx=? (car ls)
                         (bytevector-u8-ref ($port-buffer p) idx))
                       (f (fx+ i 1) (cdr ls))
                       #f)]
                  [else
                   (let ([bytes (refill-bv-buffer p who)])
                     (if (fx= bytes 0)
                         #f
                         (f i ls)))]))]))]
        [else
         (let ([bytes (refill-bv-buffer p who)])
           (if (fx= bytes 0)
               (eof-object)
               (advance-bom p who bom-seq)))]))
    ;;;
    (define (speedup-input-port p who)
      ;;; returns #t if port is eof, #f otherwise
      (unless (input-port? p)
        (die who "not an input port" p))
      (when ($port-closed? p)
        (die who "port is closed" p))
      (let ([tr ($port-transcoder p)])
        (unless tr
          (die who "not a textual port" p))
        (case (transcoder-codec tr)
          [(utf-8-codec)
           ($set-port-attrs! p
             (fxior textual-input-port-bits fast-u7-text-tag))
           (eof-object? (advance-bom p who '(#xEF #xBB #xBF)))]
          [(utf-16-codec)
           (let ([be? (advance-bom p who '(#xFE #xFF))])
             (case be?
               [(#t)
                ($set-port-attrs! p
                  (fxior textual-input-port-bits fast-u16be-text-tag))
                #f]
               [(#f)
                (let ([le? (advance-bom p who '(#xFF #xFE))])
                  (case le?
                    [(#t #f) ;;; little by default
                     ($set-port-attrs! p
                       (fxior textual-input-port-bits fast-u16le-text-tag))
                     #f]
                    [else #t]))]
               [else #t]))]
          [else
           (die who "BUG: codec not handled" (transcoder-codec tr))])))
    ;;;
    (define (lookahead-char-char-mode p who)
      (let ([str ($port-buffer p)]
            [read! ($port-read! p)])
        (if (eq? read! all-data-in-buffer)
            (eof-object)
            (let ([n (read! str 0 (string-length str))])
              (unless (fixnum? n)
                (die who "invalid return value from read!" n))
              (unless (<= 0 n (string-length str))
                (die who "return value from read! is out of range" n))
              (let ([idx ($port-index p)] [cookie ($port-cookie p)])
                (set-cookie-pos! cookie (+ idx (cookie-pos cookie))))
              ($set-port-index! p 0)
              ($set-port-size! p n)
              (cond
                [(fx= n 0)
                 (eof-object)]
                [else
                 (string-ref str 0)])))))
    ;;;
    (define (lookahead-char p)
      (define who 'lookahead-char)
      (let ([m ($port-fast-attrs p)])
        (cond
          [(eq? m fast-get-utf8-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                (let ([b (bytevector-u8-ref ($port-buffer p) i)])
                  (cond
                    [(fx< b 128) (integer->char b)]
                    [else (lookahead-char-utf8-mode p who)]))]
               [else
                (lookahead-char-utf8-mode p who)]))]
          [(eq? m fast-get-char-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                (string-ref ($port-buffer p) i)]
               [else
                (lookahead-char-char-mode p who)]))]
          [(eq? m fast-get-latin-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                (integer->char
                  (bytevector-u8-ref ($port-buffer p) i))]
               [else
                (get-char-latin-mode p who 0)]))]
          [(eq? m fast-get-utf16le-tag) (peek-utf16 p who 'little)]
          [(eq? m fast-get-utf16be-tag) (peek-utf16 p who 'big)]
          [else
           (if (speedup-input-port p who)
               (eof-object)
               (lookahead-char p))])))
    ;;;
    (define (get-char-char-mode p who)
      (let ([str ($port-buffer p)]
            [read! ($port-read! p)])
        (if (eq? read! all-data-in-buffer)
            (eof-object)
            (let ([n (read! str 0 (string-length str))])
              (unless (fixnum? n)
                (die who "invalid return value from read!" n))
              (unless (<= 0 n (string-length str))
                (die who "return value from read! is out of range" n))
              (let ([idx ($port-index p)] [cookie ($port-cookie p)])
                (set-cookie-pos! cookie (+ idx (cookie-pos cookie))))
              ($set-port-size! p n)
              (cond
                [(fx= n 0)
                 ($set-port-index! p 0)
                 (eof-object)]
                [else
                 ($set-port-index! p 1)
                 (string-ref str 0)])))))
    (define (peek-utf16 p who endianness)
      (define integer->char/invalid
        (lambda (n)
          (cond
            [(fx<= n #xD7FF)   (integer->char n)]
            [(fx< n  #xE000)   #\xFFFD]
            [(fx<= n #x10FFFF) (integer->char n)]
            [else               #\xFFFD])))
      (let ([i ($port-index p)])
        (cond
          [(fx<= (fx+ i 2) ($port-size p))
           (let ([w1 (bytevector-u16-ref ($port-buffer p) i endianness)])
             (cond
               [(or (fx< w1 #xD800) (fx> w1 #xDFFF))
                (integer->char/invalid w1)]
               [(not (and (fx<= #xD800 w1) (fx<= w1 #xDBFF)))
                #\xFFFD]
               [(fx<= (+ i 4) ($port-size p))
                (let ([w2 (bytevector-u16-ref
                            ($port-buffer p) (+ i 2) endianness)])
                  (cond
                    [(not (and (fx<= #xDC00 w2) (fx<= w2 #xDFFF)))
                     #\xFFFD]
                    [else
                     (integer->char/invalid
                       (fx+ #x10000
                         (fxlogor
                           (fxsll (fxand w1 #x3FF) 10)
                           (fxand w2 #x3FF))))]))]
               [else
                (let ([bytes (refill-bv-buffer p who)])
                  (cond
                    [(fx= bytes 0)
                      #\xFFFD]
                    [else
                     (peek-utf16 p who endianness)]))]))]
          [(fx< i ($port-size p))
           (let ([bytes (refill-bv-buffer p who)])
             (cond
               [(fx= bytes 0)
                #\xFFFD]
               [else (peek-utf16 p who endianness)]))]
          [else
           (let ([bytes (refill-bv-buffer p who)])
             (if (fx= bytes 0)
                 (eof-object)
                 (peek-utf16 p who endianness)))])))
    (define (get-utf16 p who endianness)
      (define (invalid p who endianness n)
        (case (transcoder-error-handling-mode (port-transcoder p))
          [(ignore) (do-get-char p who endianness)]
          [(replace) #\xFFFD]
          [(raise)
           (raise (make-i/o-decoding-error p n))]
          [else (die who "BUG: invalid error handling mode" p)]))
      (define (integer->char/invalid p who endianness n)
        (cond
          [(fx<= n #xD7FF)   (integer->char n)]
          [(fx< n  #xE000)   (invalid p who endianness n)]
          [(fx<= n #x10FFFF) (integer->char n)]
          [else              (invalid p who endianness n)]))
      (let ([i ($port-index p)])
        (cond
          [(fx<= (fx+ i 2) ($port-size p))
           (let ([w1 (bytevector-u16-ref ($port-buffer p) i endianness)])
             (cond
               [(or (fx< w1 #xD800) (fx> w1 #xDFFF))
                ($set-port-index! p (fx+ i 2))
                (integer->char/invalid p who endianness w1)]
               [(not (and (fx<= #xD800 w1) (fx<= w1 #xDBFF)))
                ($set-port-index! p (fx+ i 2))
                (invalid p who endianness w1)]
               [(fx<= (+ i 4) ($port-size p))
                (let ([w2 (bytevector-u16-ref
                            ($port-buffer p) (+ i 2) endianness)])
                  (cond
                    [(not (and (fx<= #xDC00 w2) (fx<= w2 #xDFFF)))
                     ($set-port-index! p (fx+ i 2))
                     (invalid p who endianness w1)]
                    [else
                     ($set-port-index! p (fx+ i 4))
                     (integer->char/invalid p who endianness
                       (fx+ #x10000
                         (fxlogor
                           (fxsll (fxand w1 #x3FF) 10)
                           (fxand w2 #x3FF))))]))]
               [else
                (let ([bytes (refill-bv-buffer p who)])
                  (cond
                    [(fx= bytes 0)
                     ($set-port-index! p ($port-size p))
                     (invalid p who endianness w1)]
                    [else
                     (get-utf16 p who endianness)]))]))]
          [(fx< i ($port-size p))
           (let ([bytes (refill-bv-buffer p who)])
             (cond
               [(fx= bytes 0)
                ($set-port-index! p ($port-size p))
                (invalid p who endianness
                  (bytevector-u8-ref ($port-buffer p) ($port-index p)))]
               [else (get-utf16 p who endianness)]))]
          [else
           (let ([bytes (refill-bv-buffer p who)])
             (if (fx= bytes 0)
                 (eof-object)
                 (get-utf16 p who endianness)))])))
    (define (get-char p)
      (do-get-char p 'get-char))
    (define read-char
      (case-lambda
        [(p) (do-get-char p 'read-char)]
        [() (do-get-char (current-input-port) 'read-char)]))
    (define (do-get-char p who)
      (let ([m ($port-fast-attrs p)])
        (cond
          [(eq? m fast-get-utf8-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                (let ([b (bytevector-u8-ref ($port-buffer p) i)])
                  (cond
                    [(fx< b 128)
                     ($set-port-index! p (fx+ i 1))
                     (if (eqv? b (char->integer #\newline))
                         (mark/return-newline p)
                         (integer->char b))]
                    [else (get-char-utf8-mode p who)]))]
               [else
                (get-char-utf8-mode p who)]))]
          [(eq? m fast-get-char-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                ($set-port-index! p (fx+ i 1))
                (let ([c (string-ref ($port-buffer p) i)])
                  (if (eqv? c #\newline)
                      (mark/return-newline p)
                      c))]
               [else (get-char-char-mode p who)]))]
          [(eq? m fast-get-latin-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                ($set-port-index! p (fx+ i 1))
                (let ([b (bytevector-u8-ref ($port-buffer p) i)])
                  (if (eqv? b (char->integer #\newline))
                      (mark/return-newline p)
                      (integer->char b)))]
               [else
                (get-char-latin-mode p who 1)]))]
         [(eq? m fast-get-utf16le-tag) (get-utf16 p who 'little)]
         [(eq? m fast-get-utf16be-tag) (get-utf16 p who 'big)]
          [else
           (if (speedup-input-port p who)
               (eof-object)
               (do-get-char p who))]))))


  ;;; ----------------------------------------------------------
  (define (assert-binary-input-port p who)
    (unless (port? p) (die who "not a port" p))
    (when ($port-closed? p) (die who "port is closed" p))
    (when ($port-transcoder p) (die who "port is not binary" p))
    (unless ($port-read! p)
      (die who "port is not an input port" p)))

  (module (get-u8 lookahead-u8)
    (import UNSAFE)
    (define (get-u8-byte-mode p who start)
      (when ($port-closed? p) (die who "port is closed" p))
      (let ([cnt (refill-bv-buffer p who)])
        (cond
          [(eqv? cnt 0) (eof-object)]
          [else
           ($set-port-index! p start)
           (bytevector-u8-ref ($port-buffer p) 0)])))
    (define (slow-get-u8 p who start)
      (assert-binary-input-port p who)
      ($set-port-attrs! p fast-get-byte-tag)
      (get-u8-byte-mode p who start))
    ;;;
    (define (get-u8 p)
      (define who 'get-u8)
      (let ([m ($port-fast-attrs p)])
        (cond
          [(eq? m fast-get-byte-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                ($set-port-index! p (fx+ i 1))
                (bytevector-u8-ref ($port-buffer p) i)]
               [else (get-u8-byte-mode p who 1)]))]
          [else (slow-get-u8 p who 1)])))
    (define (lookahead-u8 p)
      (define who 'lookahead-u8)
      (let ([m ($port-fast-attrs p)])
        (cond
          [(eq? m fast-get-byte-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                (bytevector-u8-ref ($port-buffer p) i)]
               [else (get-u8-byte-mode p who 0)]))]
          [else (slow-get-u8 p who 0)]))))

  (define (port-eof? p)
    (import UNSAFE)
    (define who 'port-eof?)
    (let ([m ($port-fast-attrs p)])
      (cond
        [(not (eq? m 0))
         (if (fx< ($port-index p) ($port-size p))
             #f
             (if ($port-transcoder p)
                 (eof-object? (lookahead-char p))
                 (eof-object? (lookahead-u8 p))))]
        [(input-port? p)
         (when ($port-closed? p)
           (die 'port-eof? "port is closed" p))
         (if (textual-port? p)
             (eof-object? (lookahead-char p))
             (eof-object? (lookahead-u8 p)))]
        [else (die 'port-eof? "not an input port" p)])))

  ;;; FIXME: these hard coded constants should go away
  (define EAGAIN-error-code -6) ;;; from ikarus-errno.c

  (define io-error
    (case-lambda
      [(who id err base-condition)
       (raise
         (condition
           base-condition
           (make-who-condition who)
           (make-message-condition (strerror err))
           (case err
             ;; from ikarus-errno.c: EACCES=-2, EFAULT=-21, EROFS=-71, EEXIST=-20,
             ;;                      EIO=-29, ENOENT=-45
             ;; Why is EFAULT included here?
             [(-2 -21) (make-i/o-file-protection-error id)]
             [(-71)    (make-i/o-file-is-read-only-error id)]
             [(-20)    (make-i/o-file-already-exists-error id)]
             [(-29)    (make-i/o-error)]
             [(-45)    (make-i/o-file-does-not-exist-error id)]
             [else (if id
                     (make-irritants-condition (list id))
                     (condition))])))]
      [(who id err) (io-error who id err (make-error))]))

  ;(define block-size 4096)
  ;(define block-size (* 4 4096))
  (define input-block-size (* 4 4096))
  (define output-block-size (* 4 4096))

  (define input-file-buffer-size (+ input-block-size 128))
  (define output-file-buffer-size output-block-size)

  (define input-socket-buffer-size
    (make-parameter (+ input-block-size 128)
      (lambda (x)
        (import (ikarus system $fx))
        (if (and (fixnum? x) ($fx>= x 128))
            x
            (error 'input-socket-buffer-size
              "buffer size should be a fixnum >= 128"
              x)))))

  (define output-socket-buffer-size
    (make-parameter output-block-size
      (lambda (x)
        (import (ikarus system $fx))
        (if (and (fixnum? x) ($fx> x 0))
            x
            (error 'output-socket-buffer-size
              "buffer size should be a positive fixnum"
              x)))))

  (define (make-file-set-position-handler fd id)
    (lambda (pos) ;;; set-position!
      (let ([err (foreign-call "ikrt_set_position" fd pos)])
        (when err
          (io-error 'set-position! id err
             (make-i/o-invalid-position-error pos))))))

  (define (fh->input-port fd id size transcoder close who)
    (letrec ([port
              ($make-port
                (input-transcoder-attrs transcoder who)
                0 0 (make-bytevector size)
                transcoder
                id
                (letrec ([refill
                          (lambda (bv idx cnt)
                            (import UNSAFE)
                            (let ([bytes
                                   (foreign-call "ikrt_read_fd" fd bv idx
                                      (if (fx< input-block-size cnt)
                                          input-block-size
                                          cnt))])
                              (cond
                                [(fx>= bytes 0) bytes]
                                [(fx= bytes EAGAIN-error-code)
                                 (call/cc
                                   (lambda (k)
                                     (add-io-event fd k 'r)
                                     (process-events)))
                                 (refill bv idx cnt)]
                                [else
                                 (io-error 'read id bytes
                                   (make-i/o-read-error))])))])
                  refill)
                #f ;;; write!
                #t ;;; get-position
                (make-file-set-position-handler fd id)
                (cond
                  [(procedure? close) close]
                  [(eqv? close #t) (file-close-proc id fd)]
                  [else #f])
                (default-cookie fd))])
    (guarded-port port)))


  (define (fh->output-port fd id size transcoder close who)
    (letrec ([port
              ($make-port
                (output-transcoder-attrs transcoder who)
                0 size (make-bytevector size)
                transcoder
                id
                #f
                (letrec ([refill
                          (lambda (bv idx cnt)
                            (import UNSAFE)
                            (let ([bytes
                                   (foreign-call "ikrt_write_fd" fd bv idx
                                     (if (fx< output-block-size cnt)
                                         output-block-size
                                         cnt))])

                              (cond
                                [(fx>= bytes 0) bytes]
                                [(fx= bytes EAGAIN-error-code)
                                 (call/cc
                                   (lambda (k)
                                     (add-io-event fd k 'w)
                                     (process-events)))
                                 (refill bv idx cnt)]
                                [else
                                 (io-error 'write id bytes
                                    (make-i/o-write-error))])))])
                  refill)
                #t ;;; get-position
                (make-file-set-position-handler fd id)
                (cond
                  [(procedure? close) close]
                  [(eqv? close #t) (file-close-proc id fd)]
                  [else #f])
                (default-cookie fd))])
      (guarded-port port)))

  (define (file-close-proc id fd)
    (lambda ()
      (cond
        [(foreign-call "ikrt_close_fd" fd) =>
         (lambda (err)
           (io-error 'close id err))])))

  (define (open-input-file-handle filename who)
    (let ([fh (foreign-call "ikrt_open_input_fd"
                 (string->utf8 filename))])
      (cond
        [(fx< fh 0) (io-error who filename fh)]
        [else fh])))

  (define (open-output-file-handle filename file-options who)
    (define (opt->num x)
      (bitwise-ior
        (if (enum-set-member? 'no-create x)   1 0)
        (if (enum-set-member? 'no-fail x)     2 0)
        (if (enum-set-member? 'no-truncate x) 4 0)))
    (let ([opt (if (enum-set? file-options)
                   (opt->num file-options)
                   (die who "file-options is not an enum set"
                        file-options))])
      (let ([fh (foreign-call "ikrt_open_output_fd"
                   (string->utf8 filename)
                   opt)])
        (cond
          [(fx< fh 0) (io-error who filename fh)]
          [else fh]))))

  (define open-file-input-port
    (case-lambda
      [(filename)
       (open-file-input-port filename (file-options) 'block #f)]
      [(filename file-options)
       (open-file-input-port filename file-options 'block #f)]
      [(filename file-options buffer-mode)
       (open-file-input-port filename file-options buffer-mode #f)]
      [(filename file-options buffer-mode transcoder)
       (define who 'open-file-input-port)
       (unless (string? filename)
         (die who "invalid filename" filename))
       (unless (enum-set? file-options)
         (die who "file-options is not an enum set" file-options))
       (unless (or (not transcoder) (transcoder? transcoder))
         (die who "invalid transcoder" transcoder))
       ; FIXME: file-options ignored
       ; FIXME: buffer-mode ignored
       (fh->input-port
         (open-input-file-handle filename who)
         filename
         input-file-buffer-size
         transcoder
         #t
         who)]))

  (define open-file-output-port
    (case-lambda
      [(filename)
       (open-file-output-port filename (file-options) 'block #f)]
      [(filename file-options)
       (open-file-output-port filename file-options 'block #f)]
      [(filename file-options buffer-mode)
       (open-file-output-port filename file-options buffer-mode #f)]
      [(filename file-options buffer-mode transcoder)
       (define who 'open-file-output-port)
       (unless (string? filename)
         (die who "invalid filename" filename))
       ; FIXME: file-options ignored
       ; FIXME: line-buffered output ports are not handled
       (unless (or (not transcoder) (transcoder? transcoder))
         (die who "invalid transcoder" transcoder))
       (let ([buffer-size
              (case buffer-mode
                [(none) 0]
                [(block line) output-file-buffer-size]
                [else (die who "invalid buffer mode" buffer-mode)])])
         (fh->output-port
           (open-output-file-handle filename file-options who)
           filename buffer-size transcoder #t who))]))

  (define (output-port-buffer-mode p)
    (unless (output-port? p)
      (die 'output-port-buffer-mode "not an output port" p))
    (if (fx= 0 ($port-size p)) 'none 'block))

  (define (open-output-file filename)
    (unless (string? filename)
      (die 'open-output-file "invalid filename" filename))
    (fh->output-port
       (open-output-file-handle filename (file-options)
          'open-output-file)
       filename
       output-file-buffer-size
       (native-transcoder)
       #t
       'open-output-file))

  (define (open-input-file filename)
    (unless (string? filename)
      (die 'open-input-file "invalid filename" filename))
    (fh->input-port
       (open-input-file-handle filename 'open-input-file)
       filename
       input-file-buffer-size
       (native-transcoder)
       #t
       'open-input-file))


  (define (with-output-to-file filename proc)
    (unless (string? filename)
      (die 'with-output-to-file "invalid filename" filename))
    (unless (procedure? proc)
      (die 'with-output-to-file "not a procedure" proc))
    (call-with-port
      (fh->output-port
        (open-output-file-handle filename (file-options)
          'with-output-to-file)
        filename
        output-file-buffer-size
        (native-transcoder)
        #t
        'with-output-to-file)
      (lambda (p)
        (parameterize ([current-output-port p])
          (proc)))))

  (define (call-with-output-file filename proc)
    (unless (string? filename)
      (die 'call-with-output-file "invalid filename" filename))
    (unless (procedure? proc)
      (die 'call-with-output-file "not a procedure" proc))
    (call-with-port
      (fh->output-port
        (open-output-file-handle filename (file-options)
          'call-with-output-file)
        filename
        output-file-buffer-size
        (native-transcoder)
        #t
        'call-with-output-file)
      proc))

  (define (call-with-input-file filename proc)
    (unless (string? filename)
      (die 'call-with-input-file "invalid filename" filename))
    (unless (procedure? proc)
      (die 'call-with-input-file "not a procedure" proc))
    (call-with-port
      (fh->input-port
        (open-input-file-handle filename 'call-with-input-file)
        filename
        input-file-buffer-size
        (native-transcoder)
        #t
        'call-with-input-file)
      proc))

  (define (with-input-from-file filename proc)
    (unless (string? filename)
      (die 'with-input-from-file "invalid filename" filename))
    (unless (procedure? proc)
      (die 'with-input-from-file "not a procedure" proc))
    (call-with-port
      (fh->input-port
        (open-input-file-handle filename 'with-input-from-file)
        filename
        input-file-buffer-size
        (native-transcoder)
        #t
        'with-input-from-file)
      (lambda (p)
        (parameterize ([current-input-port p])
          (proc)))))

  (define (with-input-from-string string proc)
    (unless (string? string)
      (die 'with-input-from-string "not a string" string))
    (unless (procedure? proc)
      (die 'with-input-from-string "not a procedure" proc))
    (parameterize ([current-input-port
                    (open-string-input-port string)])
      (proc)))

  (define (standard-input-port)
    (fh->input-port 0 '*stdin* 256 #f #f 'standard-input-port))

  (define (standard-output-port)
    (fh->output-port 1 '*stdout* 256 #f #f 'standard-output-port))

  (define (standard-error-port)
    (fh->output-port 2 '*stderr* 256 #f #f 'standard-error-port))

  (define current-input-port
    (make-parameter
      (transcoded-port
        (fh->input-port 0 '*stdin* input-file-buffer-size #f #f #f)
        (native-transcoder))
      (lambda (x)
        (if (and (input-port? x) (textual-port? x))
            x
            (die 'current-input-port "not a textual input port" x)))))

  (define current-output-port
    (make-parameter
      (transcoded-port
        (fh->output-port 1 '*stdout* output-file-buffer-size #f #f #f)
        (native-transcoder))
      (lambda (x)
        (if (and (output-port? x) (textual-port? x))
            x
            (die 'current-output-port "not a textual output port" x)))))

  (define current-error-port
    (make-parameter
      (transcoded-port
        (fh->output-port 2 '*stderr* 0 #f #f #f)
        (native-transcoder))
      (lambda (x)
        (if (and (output-port? x) (textual-port? x))
            x
            (die 'current-errorput-port "not a textual output port" x)))))

  (define console-output-port
    (let ([p (current-output-port)])
      (lambda () p)))

  (define console-error-port
    (let ([p (current-error-port)])
      (lambda () p)))

  (define console-input-port
    (let ([p (current-input-port)])
      (lambda () p)))

  (define (call-with-port p proc)
    (if (port? p)
        (if (procedure? proc)
            (call-with-values
              (lambda () (proc p))
              (lambda vals
                (close-port p)
                (apply values vals)))
            (die 'call-with-port "not a procedure" proc))
        (die 'call-with-port "not a port" p)))

  ;;;
  (define peek-char
    (case-lambda
      [() (lookahead-char (current-input-port))]
      [(p)
       (if (input-port? p)
           (if (textual-port? p)
               (lookahead-char p)
               (die 'peek-char "not a textual port" p))
           (die 'peek-char "not an input-port" p))]))

  (define (get-bytevector-n p n)
    (import (ikarus system $fx) (ikarus system $bytevectors))
    (define (subbytevector s n)
      (let ([p ($make-bytevector n)])
        (let f ([s s] [n n] [p p])
          (let ([n ($fx- n 1)])
            ($bytevector-set! p n ($bytevector-u8-ref s n))
            (if ($fx= n 0)
                p
                (f s n p))))))
    (unless (input-port? p)
      (die 'get-bytevector-n "not an input port" p))
    (unless (binary-port? p)
      (die 'get-bytevector-n "not a binary port" p))
    (unless (fixnum? n)
      (die 'get-bytevector-n "count is not a fixnum" n))
    (cond
      [($fx> n 0)
       (let ([s ($make-bytevector n)])
         (let f ([p p] [n n] [s s] [i 0])
           (let ([x (get-u8 p)])
             (cond
               [(eof-object? x)
                (if ($fx= i 0)
                    (eof-object)
                    (subbytevector s i))]
               [else
                ($bytevector-set! s i x)
                (let ([i ($fxadd1 i)])
                  (if ($fx= i n)
                      s
                      (f p n s i)))]))))]
      [($fx= n 0) '#vu8()]
      [else (die 'get-bytevector-n "count is negative" n)]))

  (define (get-bytevector-n! p s i c)
    (import (ikarus system $fx) (ikarus system $bytevectors))
    (unless (input-port? p)
      (die 'get-bytevector-n! "not an input port" p))
    (unless (binary-port? p)
      (die 'get-bytevector-n! "not a binary port" p))
    (unless (bytevector? s)
      (die 'get-bytevector-n! "not a bytevector" s))
    (let ([len ($bytevector-length s)])
      (unless (fixnum? i)
        (die 'get-bytevector-n! "starting index is not a fixnum" i))
      (when (or ($fx< i 0) ($fx> i len))
        (die 'get-bytevector-n!
          (format "starting index is out of range 0..~a" len)
          i))
      (unless (fixnum? c)
        (die 'get-bytevector-n! "count is not a fixnum" c))
      (cond
        [($fx> c 0)
         (let ([j (+ i c)])
           (when (> j len)
             (die 'get-bytevector-n!
               (format "count is out of range 0..~a" (- len i))
               c))
           (let ([x (get-u8 p)])
             (cond
               [(eof-object? x) x]
               [else
                ($bytevector-set! s i x)
                (let f ([p p] [s s] [start i] [i 1] [c c])
                  (cond
                    [($fx= i c) i]
                    [else
                     (let ([x (get-u8 p)])
                       (cond
                         [(eof-object? x) i]
                         [else
                          ($bytevector-set! s ($fx+ start i) x)
                          (f p s start ($fx+ i 1) c)]))]))])))]
        [($fx= c 0) 0]
        [else (die 'get-bytevector-n! "count is negative" c)])))

  (define (get-bytevector-some p)
    (define who 'get-bytevector-some)
;    (import UNSAFE)
    (let ([m ($port-fast-attrs p)])
      (cond
        [(eq? m fast-get-byte-tag)
         (let ([i ($port-index p)] [j ($port-size p)])
           (let ([cnt (fx- j i)])
             (cond
               [(fx> cnt 0)
                (let f ([bv (make-bytevector cnt)]
                        [buf ($port-buffer p)]
                        [i i] [j j] [idx 0])
                  (cond
                    [(fx= i j)
                     ($set-port-index! p j)
                     bv]
                    [else
                     (bytevector-u8-set! bv idx (bytevector-u8-ref buf i))
                     (f bv buf (fx+ i 1) j (fx+ idx 1))]))]
               [else
                (refill-bv-buffer p who)
                (if (fx= ($port-index p) ($port-size p))
                    (eof-object)
                    (get-bytevector-some p))])))]
        [else (die who "invalid port argument" p)])))

  (define (get-bytevector-all p)
    (define (get-it p)
      (let f ([p p] [n 0] [ac '()])
        (let ([x (get-u8 p)])
          (cond
            [(eof-object? x)
             (if (null? ac)
                 (eof-object)
                 (make-it n ac))]
            [else (f p (+ n 1) (cons x ac))]))))
    (define (make-it n revls)
      (let f ([s (make-bytevector n)] [i (- n 1)] [ls revls])
        (cond
          [(pair? ls)
           (bytevector-u8-set! s i (car ls))
           (f s (- i 1) (cdr ls))]
          [else s])))
    (if (input-port? p)
        (if (binary-port? p)
            (get-it p)
            (die 'get-bytevector-all "not a binary port" p))
        (die 'get-bytevector-all "not an input port" p)))

  (define (get-string-n p n)
    (import (ikarus system $fx) (ikarus system $strings))
    (unless (input-port? p)
      (die 'get-string-n "not an input port" p))
    (unless (textual-port? p)
      (die 'get-string-n "not a textual port" p))
    (unless (fixnum? n)
      (die 'get-string-n "count is not a fixnum" n))
    (cond
      [($fx> n 0)
       (let ([s ($make-string n)])
         (let f ([p p] [n n] [s s] [i 0])
           (let ([x (get-char p)])
             (cond
               [(eof-object? x)
                (if ($fx= i 0)
                    (eof-object)
                    (substring s 0 i))]
               [else
                ($string-set! s i x)
                (let ([i ($fxadd1 i)])
                  (if ($fx= i n)
                      s
                      (f p n s i)))]))))]
      [($fx= n 0) ""]
      [else (die 'get-string-n "count is negative" n)]))

  (define (get-string-n! p s i c)
    (import (ikarus system $fx) (ikarus system $strings))
    (unless (input-port? p)
      (die 'get-string-n! "not an input port" p))
    (unless (textual-port? p)
      (die 'get-string-n! "not a textual port" p))
    (unless (string? s)
      (die 'get-string-n! "not a string" s))
    (let ([len ($string-length s)])
      (unless (fixnum? i)
        (die 'get-string-n! "starting index is not a fixnum" i))
      (when (or ($fx< i 0) ($fx> i len))
        (die 'get-string-n!
          (format "starting index is out of range 0..~a" len)
          i))
      (unless (fixnum? c)
        (die 'get-string-n! "count is not a fixnum" c))
      (cond
        [($fx> c 0)
         (let ([j (+ i c)])
           (when (> j len)
             (die 'get-string-n!
               (format "count is out of range 0..~a" (- len i))
               c))
           (let ([x (get-char p)])
             (cond
               [(eof-object? x) x]
               [else
                ($string-set! s i x)
                (let f ([p p] [s s] [start i] [i 1] [c c])
                  (let ([x (get-char p)])
                    (cond
                      [(eof-object? x) i]
                      [else
                       ($string-set! s ($fx+ start i) x)
                       (let ([i ($fxadd1 i)])
                         (if ($fx= i c)
                             i
                             (f p s start i c)))])))])))]
        [($fx= c 0) 0]
        [else (die 'get-string-n! "count is negative" c)])))

  (define ($get-line p who)
    (import UNSAFE)
    (define (get-it p)
      (let f ([p p] [n 0] [ac '()])
        (let ([x (get-char p)])
          (cond
            [(eqv? x #\newline)
             (make-it n ac)]
            [(eof-object? x)
             (if (null? ac) x (make-it n ac))]
            [else (f p (fx+ n 1) (cons x ac))]))))
    (define (make-it n revls)
      (let f ([s (make-string n)] [i (fx- n 1)] [ls revls])
        (cond
          [(pair? ls)
           (string-set! s i (car ls))
           (f s (fx- i 1) (cdr ls))]
          [else s])))
    (if (input-port? p)
        (if (textual-port? p)
            (get-it p)
            (die who "not a textual port" p))
        (die who "not an input port" p)))
  (define (get-line p)
    ($get-line p 'get-line))
  (define read-line
    (case-lambda
      [() ($get-line (current-input-port) 'read-line)]
      [(p) ($get-line p 'read-line)]))


  (define (get-string-all p)
    (define (get-it p)
      (let f ([p p] [n 0] [ac '()])
        (let ([x (get-char p)])
          (cond
            [(eof-object? x)
             (if (null? ac)
                 (eof-object)
                 (make-it n ac))]
            [else (f p (+ n 1) (cons x ac))]))))
    (define (make-it n revls)
      (let f ([s (make-string n)] [i (- n 1)] [ls revls])
        (cond
          [(pair? ls)
           (string-set! s i (car ls))
           (f s (- i 1) (cdr ls))]
          [else s])))
    (if (input-port? p)
        (if (textual-port? p)
            (get-it p)
            (die 'get-string-all "not a textual port" p))
        (die 'get-string-all "not an input port" p)))



  ;;; ----------------------------------------------------------

  (define-syntax put-string/bv
    (syntax-rules ()
      [(_ who not-a-what pred? len $put)
       (case-lambda
         [(p bv)
          (if (pred? bv)
              ($put p bv 0 (len bv))
              (die who not-a-what bv))]
         [(p bv i)
          (if (pred? bv)
              (if (fixnum? i)
                  (let ([n (len bv)])
                    (if (and (fx<= i n) (fx>= i 0))
                        ($put p bv i (fx- n i))
                        (die who "index out of range" i)))
                  (die who "invalid index" i))
              (die who not-a-what bv))]
         [(p bv i c)
          (if (pred? bv)
              (if (fixnum? i)
                  (let ([n (len bv)])
                    (if (and (fx<= i n) (fx>= i 0))
                        (if (fixnum? c)
                            (if (and (fx>= c 0) (fx>= (fx- n c) i))
                                ($put p bv i c)
                                (die who "count out of range" c))
                            (die who "invalid count" c))
                        (die who "index out of range" i)))
                  (die who "invalid index" i))
              (die who not-a-what bv))])]))


  (module (put-char write-char put-string)
    (import UNSAFE)
    (define (put-byte! p b who)
      (let ([i ($port-index p)] [j ($port-size p)])
        (if (fx< i j)
            (begin
              (bytevector-u8-set! ($port-buffer p) i b)
              ($set-port-index! p (fx+ i 1)))
            (if (fx= j 0)
                (put-byte/unbuffered! p b who)
                (begin
                  (flush-output-port p)
                  (put-byte! p b who))))))
    (define (put-char-utf8-mode p b who)
      (cond
        [(fx< b 128)
         (put-byte! p b who)]
        [(fx<= b #x7FF)
         (put-byte! p (fxior #b11000000 (fxsra b 6)) who)
         (put-byte! p (fxior #b10000000 (fxand b #b111111)) who)]
        [(fx<= b #xFFFF)
         (put-byte! p (fxior #b11100000 (fxsra b 12)) who)
         (put-byte! p (fxior #b10000000 (fxand (fxsra b 6) #b111111)) who)
         (put-byte! p (fxior #b10000000 (fxand b #b111111)) who)]
        [else
         (put-byte! p (fxior #b11110000 (fxsra b 18)) who)
         (put-byte! p (fxior #b10000000 (fxand (fxsra b 12) #b111111)) who)
         (put-byte! p (fxior #b10000000 (fxand (fxsra b 6) #b111111)) who)
         (put-byte! p (fxior #b10000000 (fxand b #b111111)) who)]))
    ;;;
    (define write-char
      (case-lambda
        [(c p) (do-put-char p c 'write-char)]
        [(c) (do-put-char (current-output-port) c 'write-char)]))
    (define (put-char p c)
      (do-put-char p c 'put-char))
    (define ($put-string p str start count)
      (unless (output-port? p)
        (die 'put-string "not an output port" p))
      (unless (textual-port? p)
        (die 'put-string "not a textual port" p))
      (let f ([i start] [j (fx+ start count)])
        (unless (fx= i j)
          (do-put-char p (string-ref str i) 'put-string)
          (f (fx+ i 1) j))))
    (define put-string
      (put-string/bv 'put-string "not a string"
        string? string-length $put-string))
    (define (do-put-char p c who)
      (unless (char? c) (die who "not a char" c))
      (let ([m ($port-fast-attrs p)])
        (cond
          [(eq? m fast-put-utf8-tag)
           (let ([i ($port-index p)] [j ($port-size p)])
             (let ([b (char->integer c)])
               (cond
                 [(fx< b 128)
                  (if (fx< i j)
                      (begin
                        (bytevector-u8-set! ($port-buffer p) i b)
                        ($set-port-index! p (fx+ i 1)))
                      (if (fx= j 0)
                          (put-byte/unbuffered! p b who)
                          (begin
                            (flush-output-port p)
                            (put-byte! p b who))))]
                 [else
                  (put-char-utf8-mode p b who)])))]
          [(eq? m fast-put-char-tag)
           (let ([i ($port-index p)] [j ($port-size p)])
             (if (fx< i j)
                 (begin
                   (string-set! ($port-buffer p) i c)
                   ($set-port-index! p (fx+ i 1)))
                 (if (fx= j 0)
                     (put-char/unbuffered! p c who)
                     (begin
                       (flush-output-port p)
                       (do-put-char p c who)))))]
          [(eq? m fast-put-latin-tag)
           (let ([i ($port-index p)] [j ($port-size p)])
             (let ([b (char->integer c)])
               (cond
                 [(fx< b 256)
                  (if (fx< i j)
                      (begin
                        (bytevector-u8-set! ($port-buffer p) i b)
                        ($set-port-index! p (fx+ i 1)))
                      (if (fx= j 0)
                          (put-byte/unbuffered! p b who)
                          (begin
                            (flush-output-port p)
                            (put-byte! p b who))))]
                 [else
                  (case (transcoder-error-handling-mode (port-transcoder p))
                    [(ignore) (void)]
                    [(replace) (do-put-char p #\? who)]
                    [(raise)
                     (raise (make-i/o-encoding-error p c))]
                    [else (die who "BUG: invalid error handling mode" p)])])))]
          [(eq? m fast-put-utf16be-tag)
           (let ([n (char->integer c)])
             (cond
               [(fx< n #x10000)
                (put-byte! p (fxsra n 8) who)
                (put-byte! p (fxand n #xFF) who)]
               [else
                (let ([u^ (fx- n #x10000)])
                  (let ([w1 (fxior #xD800 (fxsra u^ 10))])
                    (put-byte! p (fxsra w1 8) who)
                    (put-byte! p (fxand w1 #xFF) who))
                  (let ([w2 (fxior #xDC00 (fxand u^ (- (fxsll 1 10) 1)))])
                    (put-byte! p (fxsra w2 8) who)
                    (put-byte! p (fxand w2 #xFF) who)))]))]
          [else
           (if (output-port? p)
               (if (textual-port? p)
                   (if (port-closed? p)
                       (die who "port is closed" p)
                       (die who "unsupported port" p))
                   (die who "not a textual port" p))
               (die who "not an output port" p))]))))

  (define newline
    (case-lambda
      [()
       (put-char (current-output-port) #\newline)
       (flush-output-port (current-output-port))]
      [(p)
       (unless (output-port? p)
         (die 'newline "not an output port" p))
       (unless (textual-port? p)
         (die 'newline "not a textual port" p))
       (when ($port-closed? p)
         (die 'newline "port is closed" p))
       (put-char p #\newline)
       (flush-output-port p)]))



  (module (put-u8 put-bytevector)
    (import UNSAFE)
    ;;;
    (define (put-u8 p b)
      (define who 'put-u8)
      (unless (u8? b) (die who "not a u8" b))
      (let ([m ($port-fast-attrs p)])
        (cond
          [(eq? m fast-put-byte-tag)
           (let ([i ($port-index p)] [j ($port-size p)])
             (if (fx< i j)
                 (begin
                   (bytevector-u8-set! ($port-buffer p) i b)
                   ($set-port-index! p (fx+ i 1)))
                 (if (fx= j 0)
                     (put-byte/unbuffered! p b who)
                     (begin
                       (flush-output-port p)
                       (put-u8 p b)))))]
          [else
           (if (output-port? p)
               (die who "not a binary port" p)
               (die who "not an output port" p))])))
    ;;;
    (define ($put-bytevector p bv i c)
      (define who 'put-bytevector)
      (define (copy! src dst si di c)
        (when (fx> c 0)
          (bytevector-u8-set! dst di (bytevector-u8-ref src si))
          (copy! src dst (fx+ si 1) (fx+ di 1) (fx- c 1))))
      (let ([m ($port-fast-attrs p)])
        (cond
          [(eq? m fast-put-byte-tag)
           (let ([idx ($port-index p)] [j ($port-size p)])
             (let ([room (fx- j idx)])
               (cond
                 [(fx>= room c)
                  ;; hurray
                  (copy! bv ($port-buffer p) i idx c)
                  ($set-port-index! p (fx+ idx c))]
                 [(fx> room 0)
                  ($set-port-index! p (fx+ idx room))
                  (copy! bv ($port-buffer p) i idx room)
                  (flush-output-port p)
                  ($put-bytevector p bv (fx+ i room) (fx- c room))]
                 [(fx> j 0)
                  (flush-output-port p)
                  ($put-bytevector p bv i c)]
                 [else
                  (let f ([i i] [j (fx+ i c)])
                    (unless (fx= i j)
                      (put-byte/unbuffered! p (bytevector-u8-ref bv i) who)
                      (f (fx+ i 1) j)))])))]
          [else
           (if (output-port? p)
               (die who "not a binary port" p)
               (die who "not an output port" p))])))

    (define put-bytevector
      (put-string/bv 'put-bytevector "not a bytevector"
        bytevector? bytevector-length $put-bytevector))

    ;;; module
    )



  (define (pair->env-utf8 pair)
    (let* ((key-utf8 (string->utf8 (car pair)))
           (val-utf8 (string->utf8 (cdr pair)))
           (key-len (bytevector-length key-utf8))
           (val-len (bytevector-length val-utf8))
           (result (make-bytevector (+ key-len val-len 2))))
      (bytevector-copy! key-utf8 0 result 0 key-len)
      (bytevector-u8-set! result key-len (char->integer #\=))
      (bytevector-copy! val-utf8 0 result (+ key-len 1) val-len)
      (bytevector-u8-set! result (+ key-len val-len 1) 0)
      result))

  (define (spawn-process who search? blocking? env stdin stdout stderr cmd args)
    (define (port->fd port port-pred arg-name port-type)
      (cond ((eqv? port #f) -1)
            ((port-pred port)
             (let ((fd (cookie-dest ($port-cookie port))))
               (unless (fixnum? fd)
                 (die who
                      (string-append arg-name " is not a file-based port")
                      stdin))
               fd))
            (else
             (die who
                  (string-append arg-name " is neither false nor an " port-type)
                  stdin))))
    (let ((stdin-fd (port->fd stdin input-port? "stdin" "input port"))
          (stdout-fd (port->fd stdout output-port? "stdout" "output port"))
          (stderr-fd (port->fd stderr output-port? "stderr" "output port")))
      (unless (string? cmd)
        (die who "command is not a string" cmd))
      (unless (andmap string? args)
        (die who "all command arguments must be strings"))
      (let ([r (foreign-call "ikrt_process"
                             (vector search? stdin-fd stdout-fd stderr-fd)
                             (and env (map pair->env-utf8 env))
                             (string->utf8 cmd)
                             (map string->utf8 (cons cmd args)))])
        (cond ((fixnum? r)
               (io-error who cmd r))
              (else
               (unless blocking?
                 (or stdin (set-fd-nonblocking (vector-ref r 1) who cmd))
                 (or stdout (set-fd-nonblocking (vector-ref r 2) who cmd))
                 (or stderr (set-fd-nonblocking (vector-ref r 3) who cmd)))
               (values
                (vector-ref r 0)        ; pid
                (and (not stdin)
                     (fh->output-port (vector-ref r 1)
                                      cmd output-file-buffer-size #f #t
                                      'process))
                (and (not stdout)
                     (fh->input-port (vector-ref r 2)
                                     cmd input-file-buffer-size #f #t
                                     'process))
                (and (not stderr)
                     (fh->input-port (vector-ref r 3)
                                     cmd input-file-buffer-size #f #t
                                     'process))))))))

  (define (process cmd . args)
    (spawn-process 'process #t #t #f #f #f #f cmd args))

  (define (process* search? env stdin stdout stderr cmd . args)
    (spawn-process 'process* search? #t env stdin stdout stderr cmd args))

  (define (process-nonblocking cmd . args)
    (spawn-process 'process-nonblocking #t #f #f #f #f cmd args))

  (define (set-fd-nonblocking fd who id)
    (let ([rv (foreign-call "ikrt_make_fd_nonblocking" fd)])
      (unless (eq? rv 0)
        (io-error who id rv))))

  (define (socket->ports socket who id block?)
    (if (< socket 0)
        (io-error who id socket)
        (let ([close
               (let ([closed-once? #f])
                 (lambda ()
                   (if closed-once?
                       ((file-close-proc id socket))
                       (set! closed-once? #t))))])
          (unless block?
            (set-fd-nonblocking socket who id))
          (values
            (fh->input-port socket
               id (input-socket-buffer-size) #f close who)
            (fh->output-port socket
               id (output-socket-buffer-size) #f close who)))))

  (define-syntax define-connector
    (syntax-rules ()
      [(_ who foreign-name block?)
       (define (who host srvc)
         (unless (and (string? host) (string? srvc))
           (die 'who "host and service must both be strings" host srvc))
         (socket->ports
           (or (foreign-call foreign-name
                 (string->utf8 host) (string->utf8 srvc))
               (die 'who "failed to resolve host name or connect" host srvc))
           'who
           (string-append host ":" srvc)
           block?))]))

  (define-connector tcp-connect             "ikrt_tcp_connect" #t)
  (define-connector udp-connect             "ikrt_udp_connect" #t)
  (define-connector tcp-connect-nonblocking "ikrt_tcp_connect" #f)
  (define-connector udp-connect-nonblocking "ikrt_udp_connect" #f)

  (module (add-io-event rem-io-event process-events)
    (define-struct t (fd proc type))
    ;;; callbacks
    (define pending '())
    (define out-queue '())
    (define in-queue '())

    (define (process-events)
      (if (null? out-queue)
          (if (null? in-queue)
              (if (null? pending)
                  (error 'process-events "no more events")
                  (begin
                    (do-select)
                    (process-events)))
              (begin
                (set! out-queue (reverse in-queue))
                (set! in-queue '())
                (process-events)))
          (let ([t (car out-queue)])
            (set! out-queue (cdr out-queue))
            ((t-proc t))
            (process-events))))

    (define (add-io-event fd proc event-type)
      (set! pending
        (cons (make-t fd proc event-type) pending)))

    (define (rem-io-event fd)
      (define (p x) (eq? (t-fd x) fd))
      (set! pending (remp p pending))
      (set! out-queue (remp p out-queue))
      (set! in-queue (remp p in-queue)))

    (define (get-max-fd)
      (assert (pair? pending))
      (let f ([m (t-fd (car pending))]
              [ls (cdr pending)])
        (cond
          [(null? ls) m]
          [else (f (max m (t-fd (car ls))) (cdr ls))])))

    (define (do-select)
      (let ([n (add1 (get-max-fd))])
        (let ([vecsize (div (+ n 7) 8)])
          (let ([rbv (make-bytevector vecsize 0)]
                [wbv (make-bytevector vecsize 0)]
                [xbv (make-bytevector vecsize 0)])
            ;;; add all fds to their bytevectors depending on type
            (for-each
              (lambda (t)
                (let ([fd (t-fd t)])
                  (let ([i (div fd 8)] [j (mod fd 8)])
                    (let ([bv (case (t-type t)
                                [(r) rbv]
                                [(w) wbv]
                                [(x) xbv]
                                [else
                                 (error 'do-select "invalid type" t)])])
                      (bytevector-u8-set! bv i
                        (fxlogor (fxsll 1 j)
                          (bytevector-u8-ref bv i)))))))
              pending)
            ;;; do select
            (let ([rv (foreign-call "ikrt_select" n rbv wbv xbv)])
              (when (< rv 0)
                (io-error 'select #f rv)))
            ;;; go through fds again and see if they're selected
            (for-each
              (lambda (t)
                (let ([fd (t-fd t)])
                  (let ([i (div fd 8)] [j (mod fd 8)])
                    (let ([bv (case (t-type t)
                                [(r) rbv]
                                [(w) wbv]
                                [(x) xbv]
                                [else
                                 (error 'do-select "invalid type" t)])])
                      (cond
                        [(fxzero?
                           (fxlogand (fxsll 1 j)
                             (bytevector-u8-ref bv i)))
                         ;;; not selected
                         (set! pending (cons t pending))]
                        [else
                         ;;; ready
                         (set! in-queue (cons t in-queue))])))))
              (let ([ls pending])
                (set! pending '())
                ls))))))
    )


  (define-struct tcp-server (portnum fd))

  (define (tcp-server-socket portnum)
    (unless (fixnum? portnum)
      (error 'tcp-server-socket "not a fixnum" portnum))
    (let ([sock (foreign-call "ikrt_listen" portnum)])
      (cond
        [(fx>= sock 0) (make-tcp-server portnum sock)]
        [else (die 'tcp-server-socket "failed to start server")])))

  (define (tcp-server-socket-nonblocking portnum)
    (let ([s (tcp-server-socket portnum)])
      (set-fd-nonblocking (tcp-server-fd s)
        'tcp-server-socket-nonblocking
        '#f)
      s))


  (define (do-accept-connection s who blocking?)
    (define (make-socket-info x)
      (unless (= (bytevector-length x) 16)
        (error who "BUG: unexpected return value" x))
      (format "~s.~s.~s.~s:~s"
        (bytevector-u8-ref x 4)
        (bytevector-u8-ref x 5)
        (bytevector-u8-ref x 6)
        (bytevector-u8-ref x 7)
        (+ (* 256 (bytevector-u8-ref x 2))
           (bytevector-u8-ref x 3))))
    (unless (tcp-server? s)
      (die who "not a tcp server" s))
    (let ([fd (tcp-server-fd s)] [bv (make-bytevector 16)])
      (unless fd
        (die who "server is closed" s))
      (let ([sock (foreign-call "ikrt_accept" fd bv)])
        (cond
          [(eq? sock EAGAIN-error-code)
           (call/cc
             (lambda (k)
               (add-io-event fd k 'r)
               (process-events)))
           (do-accept-connection s who blocking?)]
          [(< sock 0)
           (io-error who s sock)]
          [else
           (socket->ports sock who (make-socket-info bv) blocking?)]))))

  (define (accept-connection s)
    (do-accept-connection s 'accept-connection #t))

  (define (accept-connection-nonblocking s)
    (do-accept-connection s 'accept-connection-nonblocking #f))

  (define (close-tcp-server-socket s)
    (define who 'close-tcp-server-socket)
    (unless (tcp-server? s)
      (die who "not a tcp server" s))
    (let ([fd (tcp-server-fd s)])
      (unless fd
        (die who "server is closed" s))
      (let ([rv (foreign-call "ikrt_shutdown" fd)])
        (when (fx< rv 0)
          (die who "failed to shutdown")))))

  (define (unregister-callback what)
    (define who 'unregister-callback)
    (cond
      [(output-port? what)
       (let ([c (cookie-dest ($port-cookie what))])
         (unless (fixnum? c) (die who "not a file-based port" what))
         (rem-io-event c))]
      [(input-port? what)
       (let ([c (cookie-dest ($port-cookie what))])
         (unless (fixnum? c) (die who "not a file-based port" what))
         (rem-io-event c))]
      [(tcp-server? what)
       (rem-io-event (tcp-server-fd what))]
      [else (die who "invalid argument" what)]))

  (define (register-callback what proc)
    (define who 'register-callback)
    (unless (procedure? proc)
      (die who "not a procedure" proc))
    (cond
      [(output-port? what)
       (let ([c (cookie-dest ($port-cookie what))])
         (unless (fixnum? c) (die who "not a file-based port" what))
         (add-io-event c proc 'w))]
      [(input-port? what)
       (let ([c (cookie-dest ($port-cookie what))])
         (unless (fixnum? c) (die who "not a file-based port" what))
         (add-io-event c proc 'r))]
      [(tcp-server? what)
       (add-io-event (tcp-server-fd what) proc 'r)]
      [else (die who "invalid argument" what)]))


  (module (directory-stream? open-directory-stream
           read-directory-stream close-directory-stream)

    (define-struct directory-stream (filename pointer closed?))

    (define G (make-guardian))

    (define (clean-up)
      (cond
        [(G) =>
         (lambda (x)
           (close-directory-stream x #f)
           (clean-up))]))

    (define (open-directory-stream filename)
      (define who 'open-directory-stream)
      (unless (string? filename)
        (die who "not a string" filename))
      (clean-up)
      (let ([rv (foreign-call "ikrt_opendir" (string->utf8 filename))])
        (if (fixnum? rv)
            (io-error who filename rv)
            (let ([stream (make-directory-stream filename rv #f)])
              (G stream)
              stream))))

    (define (read-directory-stream x)
      (define who 'read-directory-stream)
      (unless (directory-stream? x)
        (die who "not a directory stream" x))
      (when (directory-stream-closed? x)
        (die who "directory stream is closed" x))
      (let ([rv (foreign-call "ikrt_readdir"
                   (directory-stream-pointer x))])
        (cond
          [(fixnum? rv)
           (close-directory-stream x #f)
           (io-error who (directory-stream-filename x) rv)]
          [(not rv) #f]
          [else (utf8->string rv)])))

    (define close-directory-stream
      (case-lambda
        [(x wanterror?)
         (define who 'close-directory-stream)
         (clean-up)
         (unless (directory-stream? x)
           (die who "not a directory stream" x))
         (unless (directory-stream-closed? x)
           (set-directory-stream-closed?! x #t)
           (let ([rv (foreign-call "ikrt_closedir"
                       (directory-stream-pointer x))])
             (when (and wanterror? (not (eqv? rv 0)))
               (io-error who (directory-stream-filename x) rv))))]
        [(x) (close-directory-stream x #t)]))

    (set-rtd-printer! (type-descriptor directory-stream)
      (lambda (x p wr)
        (fprintf p "#<directory-stream ~a>"
           (directory-stream-filename x)))))


  ;(set-fd-nonblocking 0 'init '*stdin*)
  )

