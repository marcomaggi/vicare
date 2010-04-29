
(library (tests scribble)
  (export run-tests)

  (import (ikarus))

  (define (run-tests) (test-scribble))

  (define (test-scribble)

    (define failed 0)
    (define passed 0)
    
    (define (test-one str expected)
      (guard (con
               [else
                (printf "======================================\n")
                (display "testing scribble on:\n")
                (display str)
                (newline)
                (printf "reads as\n")
                (pretty-print expected)
                (printf "test failed!\n")
                (print-condition con)
                (set! failed (+ failed 1))
                (printf "FAILED ~s tests, PASSED ~s\n" failed passed)])
        (let ([p (open-string-input-port str)])
          (let ([v (read p)])
            (unless (equal? v expected)
              (error 'test "mismatch" v)))
          (let ([v (read p)])
            (unless (eof-object? v)
              (error 'test "not eof" v))))
        (set! passed (+ passed 1))
        (printf " [~s]" passed)))
    
    (define-syntax tests
      (lambda (x)
        (define (process ls)
          (cond
            [(null? ls) #'(values)]
            [else
             (let ([x (syntax->datum (car ls))])
               (assert (string? x))
               (let f ([ac x] [ls (cdr ls)])
                 (syntax-case ls (reads as)
                   [(y rest ...) (string? (syntax->datum #'y))
                    (f (string-append ac "\n" (syntax->datum #'y))
                       #'(rest ...))]
                   [(reads as foo rest ...)
                    (with-syntax ([ac ac]
                                  [rest (process #'(rest ...))])
                      #'(begin (test-one ac 'foo) rest))])))]))
        (syntax-case x ()
          [(_ ls ...) 
           (process #'(ls ...))])))
    
    (tests
      "@foo{blah blah blah}" 
        reads as 
      (foo "blah blah blah")
    
      "@foo{blah \"blah\" (`blah'?)}"
        reads as 
      (foo "blah \"blah\" (`blah'?)")
    
      "@foo[1 2]{3 4}"
        reads as
      (foo 1 2 "3 4")
    
      "@foo[1 2 3 4]"
        reads as
      (foo 1 2 3 4)
    
      "@foo[:width 2]{blah blah}"
        reads as
      (foo :width 2 "blah blah")
    
      "@foo{blah blah"
      "     yada yada}"
        reads as
      (foo "blah blah" "\n" "yada yada")
    
      "@foo{"
      "  blah blah"
      "  yada yada"
      "}"
        reads as
      (foo "blah blah" "\n" "yada yada")
    
      "@foo{bar @baz{3}"
      "     blah}"
        reads as
      (foo "bar " (baz "3") "\n" "blah")
        
      "@foo{@b{@u[3] @u{4}}"
      "     blah}"
        reads as
      (foo (b (u 3) " " (u "4")) "\n" "blah")
    
      "@C{while (*(p++))"
      "     *p = '\\n';}"
        reads as 
      (C "while (*(p++))" "\n" "  " "*p = '\\n';")
    
      "@{blah blah}"
        reads as
      ("blah blah")
    
      "@{blah @[3]}"
        reads as
      ("blah " (3))
    
      "'@{foo"
      "   bar"
      "   baz}"
        reads as
      '("foo" "\n" "bar" "\n" "baz")
    
      "@foo"
        reads as
      foo
    
      "@{blah @foo blah}"
        reads as
      ("blah " foo " blah")
            
      "@{blah @foo: blah}"
        reads as
      ("blah " foo: " blah")
              
      "@{blah @|foo|: blah}"
        reads as 
      ("blah " foo ": blah")
    
      "@foo{(+ 1 2) -> @(+ 1 2)!}"
        reads as  
      (foo "(+ 1 2) -> " (+ 1 2) "!")
          
      "@foo{A @\"string\" escape}"
        reads as  
      (foo "A string escape")
    
      "@foo{eli@\"@\"barzilay.org}"
        reads as
      (foo "eli@barzilay.org")
        
      "@foo{A @\"{\" begins a block}"
        reads as  
      (foo "A { begins a block")
        
      "@C{while (*(p++)) {"
      "     *p = '\\n';"
      "   }}"
        reads as  
      (C "while (*(p++)) {" "\n" "  "
         "*p = '\\n';" "\n"
         "}")
        
      "@foo|{bar}@{baz}|"
        reads as  
      (foo "bar}@{baz")
        
      "@foo|{bar |@x{X} baz}|"
        reads as  
      (foo "bar " (x "X") " baz")
        
      "@foo|{bar |@x|{@}| baz}|"
        reads as  
      (foo "bar " (x "@") " baz")
      
      "@foo|--{bar}@|{baz}--|"
        reads as  
      (foo "bar}@|{baz")
        
      "@foo|<<{bar}@|{baz}>>|"
        reads as  
      (foo "bar}@|{baz")
      
      ;;; ikarus does not allow \@identifier  
      "(define |@email| \"foo@bar.com\")"
        reads as  
      (define |@email| "foo@bar.com")
        
      "(define |@atchar| #\\@)"
        reads as  
      (define |@atchar| #\@)
      
      "@foo{bar @baz[2 3] {4 5}}"
        reads as  
      (foo "bar " (baz 2 3) " {4 5}")
        
      "@{foo bar"
      "  baz}"
        reads as  
      ("foo bar" "\n" "baz")
        
      "@foo{x @y z}"
        reads as  
      (foo "x " y " z")
        
      "@foo{x @(* y 2) z}"
        reads as  
      (foo "x " (* y 2) " z")
        
      "@{@foo bar}"
        reads as  
      (foo " bar")
        
      "@@foo{bar}{baz}"
        reads as  
      ((foo "bar") "baz")
        
      "@foo[1 (* 2 3)]{bar}"
        reads as  
      (foo 1 (* 2 3) "bar")
        
      "@foo[@bar{...}]{blah}"
        reads as  
      (foo (bar "...") "blah")
        
      "@foo[bar]"
        reads as  
      (foo bar)
        
      "@foo{bar @f[x] baz}"
        reads as  
      (foo "bar " (f x) " baz")
        
      "@foo[]{bar}"
        reads as  
      (foo "bar")
        
      "@foo[]"
        reads as  
      (foo)
        
      "@foo"
        reads as  
      foo
        
      "@foo{}"
        reads as  
      (foo)
        
      "@foo[:style 'big]{bar}"
        reads as  
      (foo :style 'big  "bar")
        
      "@foo{f{o}o}"
        reads as  
      (foo "f{o}o")
        
      "@foo{{{}}{}}"
        reads as  
      (foo "{{}}{}")
        
      "@foo{bar}"
        reads as  
      (foo "bar")
        
      "@foo{ bar }"
        reads as  
      (foo " bar ")
        
      "@foo[1]{ bar }"
        reads as  
      (foo 1 " bar ")
      
      "@foo{a @bar{b} c}"
        reads as  
      (foo "a " (bar "b") " c")
        
      "@foo{a @bar c}"
        reads as  
      (foo "a " bar " c")
        
      "@foo{a @(bar 2) c}"
        reads as  
      (foo "a " (bar 2) " c")
      
      "@foo{A @\"}\" marks the end}"
        reads as  
      (foo "A } marks the end")
        
      "@foo{The prefix: @\"@\".}"
        reads as  
      (foo "The prefix: @.")
        
      "@foo{@\"@x{y}\" --> (x \"y\")}"
        reads as  
      (foo "@x{y} --> (x \"y\")")
        
      "@foo|{...}|"
        reads as  
      (foo "...")
        
      "@foo|{\"}\" follows \"{\"}|"
        reads as  
      (foo "\"}\" follows \"{\"")
        
      "@foo|{Nesting |{is}| ok}|"
        reads as  
      (foo "Nesting |{is}| ok")
      
      "@foo|{Maze"
      "      |@bar{is}"
      "      Life!}|"
        reads as  
      (foo "Maze" "\n"
           (bar "is") "\n"
            "Life!")
    
      "@t|{In |@i|{sub|@\"@\"s}| too}|"
        reads as  
      (t "In " (i "sub@s") " too")
      
      "@foo|<<<{@x{foo} |@{bar}|.}>>>|"
        reads as  
      (foo "@x{foo} |@{bar}|.")
        
      "@foo|!!{X |!!@b{Y}...}!!|"
        reads as  
      (foo "X " (b "Y") "...")
        
      "@foo{foo@bar.}"
        reads as  
      (foo "foo" bar.)
        
      "@foo{foo@|bar|.}"
        reads as  
      (foo "foo" bar ".")
        
      "@foo{foo@3.}"
        reads as  
      (foo "foo" 3.0)
        
      "@foo{foo@| 3 |.}"
        reads as
      (foo "foo" 3 ".")
        
      "@foo{foo@|(f 1)|{bar}}"
        reads as  
      (foo "foo" (f 1) "{bar}")
        
      "@foo{foo@|bar|[1]{baz}}"
        reads as  
      (foo "foo" bar "[1]{baz}")
        
      "@foo{x@\"y\"z}"
        reads as  
      (foo "xyz")
        
      "@foo{x@|\"y\"|z}"
        reads as  
      (foo "x" "y" "z")
        
      "@foo{x@| 1 (+ 2 3) 4 |y}"
        reads as  
      (foo "x" 1 (+ 2 3) 4 "y")
        
      "@foo{x@|*"
      "        *|y}"
        reads as  
      (foo "x" * * "y")
        
      "@foo{Alice@||Bob@|"
      "     |Carol}"
        reads as  
      (foo "Alice" "Bob" "Carol")
      
      "@|{blah}|"
        reads as  
      ("blah")
      
      "@foo{bar}"
        reads as  
      (foo "bar")
        
      "@foo{ bar }"
        reads as  
      (foo " bar ")
        
      "@foo{ bar"
      "     baz }"
        reads as  
      (foo " bar" "\n" "baz ")
        
      "@foo{bar"
      "}"
        reads as  
      (foo "bar")
        
        
      "@foo{"
      "  bar"
      "}"
        reads as  
      (foo "bar")
        
      "@foo{"
      " "
      "  bar"
      " "
      "}"
        reads as  
      (foo "\n" "bar" "\n")
        
      "@foo{"
      "  bar"
      " "
      "  baz"
      "}"
        reads as  
      (foo "bar" "\n" "\n" "baz")
        
      "@foo{"
      "}"
        reads as  
      (foo "\n")
        
      "@foo{"
      " "
      "}"
        reads as  
      (foo "\n" "\n")
        
      "@foo{ bar"
      "     baz }"
        reads as  
      (foo " bar" "\n" "baz ")
      
      "@foo{"
      "  bar"
      "  baz"
      "  blah"
      "}"
        reads as  
      (foo "bar" "\n" "baz" "\n" "blah")
        
      "@foo{"
      "  begin"
      "    x++;"
      "  end}"
        reads as  
      (foo "begin" "\n" "  " "x++;" "\n" "end")
        
      "@foo{"
      "    a"
      "   b"
      "  c}"
        reads as  
      (foo "  " "a" "\n" " " "b" "\n" "c")
        
      "@foo{bar"
      "       baz"
      "     bbb}"
        reads as  
      (foo "bar" "\n" "  " "baz" "\n" "bbb")
        
      "@foo{ bar"
      "        baz"
      "      bbb}"
        reads as  
      (foo " bar" "\n" "   " "baz" "\n" " " "bbb")
        
      "@foo{bar"
      "   baz"
      "   bbb}"
        reads as  
      (foo "bar" "\n" "baz" "\n" "bbb")
        
      "@foo{ bar"
      "   baz"
      "   bbb}"
        reads as  
      (foo " bar" "\n" "baz" "\n" "bbb")
        
      "@foo{ bar"
      "   baz"
      "     bbb}"
        reads as  
      (foo " bar" "\n" "baz" "\n" "  " "bbb")
        
      "@text{Some @b{bold"
      "  text}, and"
      "  more text.}"
        reads as  
      (text "Some " (b "bold" "\n" "text")", and" "\n" "more text.")
      
      "@foo{"
      "  @|| bar @||"
      "  @|| baz}"
        reads as  
      (foo " bar " "\n" " baz")
      
      "@foo{@|xyz|}"
      reads as 
      (foo xyz)
    
      "@foo{@|<xyz>|}"
      reads as 
      (foo <xyz>)
    
      "@foo{@|<<<<|}"
      reads as 
      (foo <<<<)
    
      "@foo{@|<(x)>|}"
      reads as
      (foo < (x) >)
    
      "@foo{@|(<(<<)>) xy|}"
      reads as
      (foo (< (<<) >) xy)
    
    
    )
    (assert (= failed 0))))



