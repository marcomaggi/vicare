

;;; WORK IN PROGRESS, NOT FOR CONSUMPTION
;;; TODO: long options
;;;       multiple options in one go, e.g., -XYZ
;;;       concat value with option, e.g., -Xxvalue  
;;;       usage error message [ok?]
;;;       -h --help should not be user-defined
;;;       check duplicate options


(import (ikarus) (command-line))

((pretty-format 'command-line-interface)
  '(_ ls tab [0 e ...] ...))

(define-syntax define-command
  (syntax-rules ()
    [(_ (name arg) . body) 
     (begin
       (define (name arg) . body)
       (display "================================================\n")
       (pretty-print '(define (name arg) . body))
       (newline))]))

(define-syntax test 
  (syntax-rules ()
    [(_ command 
        [ls* expected*] ...)
     (for-each
       (lambda (ls expected)
         (printf "~s => " `(command ',ls))
         (let ([a1 (command ls)]
               [a2 expected])
           (unless (equal? a1 a2)
             (error #f "failed/got/expected" a1 expected))
           (printf "~s OK\n" expected)))
       '(ls* ...)
       '(expected* ...))]))

(define-command (command1 ls)
  (command-line-interface ls
    [(p)          "Help0" (list 0 p)]
    [(p p1)       "Help1" (list 1 p p1)]
    [(p p1 p2)    "Help2" (list 2 p p1 p2)]
    [(p p1 p2 p3) "Help3" (list 3 p p1 p2 p3)]))

(test command1
  [("p")                 (0 "p")]
  [("p" "p1")            (1 "p" "p1")]
  [("p" "p1" "p2")       (2 "p" "p1" "p2")]
  [("p" "p1" "p2" "p3")  (3 "p" "p1" "p2" "p3")]
  [("./prog" "p1" "p2" "p3" "p4")  #f])

(define-command (command2 ls)
  (command-line-interface ls
    [(p p1 p2 p3) "Help3" (list 3 p p1 p2 p3)]
    [(p p1 p2 ps ...) "Help2" (list 2 p p1 p2 ps)]
    [(p p1 ps ...) "Help1" (list 1 p p1 ps)]
    [(p ps ...) "Help0" (list 0 p ps)]))

(test command2
  [("p")                 (0 "p" ())]
  [("p" "a")             (1 "p" "a" ())]
  [("p" "a" "b")         (2 "p" "a" "b" ())]
  [("p" "a" "b" "c")     (3 "p" "a" "b" "c")]
  [("p" "a" "b" "c" "d") (2 "p" "a" "b" ("c" "d"))]
  [("./prog" "-h") #f])

(define-command (command3 ls)
  (command-line-interface ls
    [(p "-X" xopt "-Y" yopt) (list 'xy p xopt yopt)]
    [(p "-X" xopt) (list 'x p xopt)]
    [(p "-Y" yopt) (list 'y p yopt)]))

(test command3
  [("p" "-X" "xopt") (x "p" "xopt")]
  [("p" "-Y" "yopt") (y "p" "yopt")]
  [("p" "-X" "xopt" "-Y" "yopt") (xy "p" "xopt" "yopt")]
  [("p" "-Y" "yopt" "-X" "xopt") (xy "p" "xopt" "yopt")]
  [("./prog") #f]
  [("./prog" "-h") #f])

(define-command (command4 ls)
  (command-line-interface ls
    [(p "-X?" xopt "-Y?" yopt) (list p xopt yopt)]
    [(p "-X?" xopt rest ...)   (list p xopt rest)]))

(test command4
  [("p")            ("p" #f #f)]
  [("p" "-X")       ("p" #t #f)]
  [("p" "-Y")       ("p" #f #t)]
  [("p" "-X" "-Y")  ("p" #t #t)]
  [("p" "-Y" "-X")  ("p" #t #t)]
  [("p" "-X" "a")   ("p" #t ("a"))]
  [("p" "a")        ("p" #f ("a"))]
  [("./prog" "-h")  #f])

(define-command (command5 ls)
  (command-line-interface ls
    [(p "-X=default" xopt) (list p xopt)]))

(test command5
  [("p")              ("p" "default")]
  [("p" "-X" "hello") ("p" "hello")]
  [("./prog" "-h")         #f])

(define-command (command6 ls)
  (command-line-interface ls
    [(p "-X*" xopts) (list p xopts)]
    [(p "-X*" xopts "-Y*" yopts) (list p xopts yopts)]))

(test command6
  [("p")              ("p" ())]
  [("p" "-X" "a" "-X" "b") ("p" ("a" "b"))]
  [("p" "-X" "a" "-Y" "b") ("p" ("a") ("b"))]
  [("p" "-Y" "b") ("p" () ("b"))]
  [("p" "-X" "a" "-Y" "b" "-X" "c" "-Y" "d") ("p" ("a" "c") ("b" "d"))]
  [("./prog" "-Q" "12") #f]
  [("./prog" "-h") #f])

(define-command (command7 ls)
  (command-line-interface ls
    [(p "-X+" xopts) (list p xopts)]
    [(p "-X*" xopts "-Y+" yopts) (list p xopts yopts)]))

(test command7
  [("p" "-X" "a") ("p" ("a"))]
  [("p" "-X" "a" "-X" "b") ("p" ("a" "b"))]
  [("p" "-X" "a" "-Y" "b") ("p" ("a") ("b"))]
  [("p" "-Y" "b") ("p" () ("b"))]
  [("p" "-X" "a" "-Y" "b" "-X" "c" "-Y" "d") ("p" ("a" "c") ("b" "d"))]
  [("./prog") #f]
  [("./prog" "-h") #f])

(define-command (command8 ls)
  (command-line-interface ls
    [(p "-Q=foobar" q "-R=blabla" r "-X?" xopts "-Y?" yopt "-L*" libs 
        "-f" file file* ...) 
     "Does something nice" 
     #t]))

(test command8
  [("./prog") #f]
  [("./prog" "-h") #f]
  [("./prog" "--help") #f])



(define-command (ls-command ls)
  (command-line-interface ls
    [(ls "-A?" A "-B?" B "-C?" C "-F?" F "-G?" G "-H?" H "-L?" L
         "-P?" P "-R?" R "-S?" S "-T?" T "-W?" W "-Z?" Z "-a?" a
         "-b?" b "-c?" c "-d?" d "-e?" e "-f?" f "-g?" g "-i?" i
         "-k?" k "-l?" l "-m?" m "-n?" n "-o?" o "-p?" p "-q?" q
         "-r?" r "-s?" s "-t?" t "-u?" u "-w?" w "-x?" x "-1?" o1 
         files ...)
     #t]))

(test ls-command
  [("ls" "-h") #f])


#!eof
(define (real-test ls)
  (command-line-interface ls
    (options:
      [("-O" "--optimize-level")
       "Specify optimization level"
       numeric-option
       (lambda (n err)
         (if (memv n '(0 1 2))
             (begin (optimize-level n) n)
             (err "valid options are 0, 1, and 2")))]
      [("-L" "--library-path")
       "Adds path to library search path"
       (lambda (s err)
         (library-path (cons s (library-path))))])
    [(program "-O1" "-L" libdirs ... "-l" library-files ...
              "--r6rs-script" script-file args ...)
     "Run <script-file> in R6RS-script mode."
     (list 1 program libdirs library-files script-file args)]
    [(program "-O2" "-L" libdirs ... 
              "--compile-dependencies" script-file)
     "Compile all libraries that <script-file> depends on"
     (list 2 program libdirs script-file)]
    [(program "-O0" "-L" libdirs ... "-l" library-files ...
              init-files ... "--script" script-file args ...)
     "Run <script-file> in R5RS-script mode"
      "Each of <library-files> must contain a library and are"
      "installed before <init-files> are loaded and"
      "<script-file> is run."
     (list 3 program libdirs library-files init-files script-file args)]
    [(program "-O0" "-L" libdirs ... "-l" library-files init-files ... 
              "--" args ...)
     "Run Ikarus in interactive mode."
     "Each of <library-files> must contain a library and are"
     "installed before the <init-files> are loaded"
     (list 4 program libdirs library-files init-files args)]))

#!eof


=head1 NAME

gimp-request - send a request to GIMP's Script-Fu Server

=head1 SYNOPSIS

Syntax:

    $ gimp-request \
      [--server=HOST][--port=PORT] \
      [SCHEME_FILE] [ARGS]...

