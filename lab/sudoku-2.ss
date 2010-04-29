;;; Rich Lewis
;;; Sudoku Puzzle Solver Version 2.0

;;; Hopefully someday, I'll work out a better user interface, but it
;;; serves its purpose for now.  Feel free to pass it around to anyone
;;; who is interested in sudoku puzzles and wants a solver written in
;;; scheme.
;;; 
;;; Rich Lewis

;(print-vector-length #f)
(define make-table
  (lambda (nr nc init)
    (let ([tbl (make-vector nr)])
      (let insert-rows! ([i 0])
        (unless (= i nr)
          (vector-set! tbl i (make-vector nc init))
          (insert-rows! (+ i 1))))
      tbl)))

(define table-set!
  (lambda (tbl ri ci x)
    (vector-set! (vector-ref tbl ri) ci x)))

(define table-ref
  (lambda (tbl ri ci)
    (vector-ref (vector-ref tbl ri) ci)))

(define make-grid
  (lambda ()
    (let ([grid (make-table 9 9 #f)])
      (let row-loop ([row 0])
        (if (= row 9)
            grid
            (let col-loop ([col 0])
              (if (= col 9)
                  (row-loop (+ row 1))
                  (begin
                    (table-set! grid row col (make-vector 9 1))
                    (col-loop (+ col 1))))))))))

(define grid (make-grid))

(define grid-ref
  (lambda (x y)
    (table-ref grid y x)))

(define grid-set! 
  (lambda (x y val)
    (table-set! grid y x val)))

(define reset-row
  (lambda (row)
    (let loop ([i 0])
      (unless (= i 9)
        (grid-set! row i (make-vector 9 1))
        (loop (+ i 1))))))

(define reset-grid
  (lambda ()
    (let row-loop ([row 0])
      (unless (= row 9)
        (let col-loop ([col 0])
          (if (= col 9)
              (row-loop (+ row 1))
              (begin
                (grid-set! row col (make-vector 9 1))
                (col-loop (+ col 1)))))))))

(define input-grid
  (lambda ()
    (printf "                 Sudoku Puzzle Solver~%~%")
    (printf "Starting in the top left-hand corner, input either a 0 for a~%")
    (printf "blank space or a number between 1 and 9.  Then press enter and~%")
    (printf "repeat across the first row.  Return to the beginning of the~%")
    (printf "next row and input each row until the end. If a number is~%")
    (printf "mis-entered, inputing \"s\" will return to the beginning. \"r\" will~%")
    (printf "restart the current row and \"q\" will quit the program altogether.~%~%")
    (printf "When the last cell (9,9) is entered, the puzzle will automatically~%")
    (printf "be solved and the result printed in a simple grid.~%~%")
    (let row-loop ([i 0])
      (unless (= i 9)
        (let col-loop ([j 0])
          (if (= j 9)
              (begin
                (newline)
                (row-loop (+ i 1)))
              (begin                                
                (printf "cell ~a, ~a:   " (+ i 1) (+ j 1))
                (let ([c (read)])
                  (cond
                    [(and (number? c) (> c 0) (< c 10)) (begin
                                                          (grid-set! i j c)
                                                          (col-loop (+ j 1)))]
                    [(equal? c '0) (begin
                                     (grid-set! i j (make-vector 9 1))
                                     (col-loop (+ j 1)))]
                    [(equal? c 'q) (row-loop 9)]
                    [(equal? c 'r) (begin
                                     (reset-row i)
                                     (col-loop 0))]
                    [(equal? c 's) (begin
                                     (reset-grid)
                                     (row-loop 0))]
                    [else (begin 
                            (printf "invalid input ~a~%" c)
                            (col-loop j))])))))))))

(define input-block
  (lambda (block)
    (unless (= (length block) 10) 
      (error 'input-block "invalid block"))
    (let f ([i 0] [ls (cdr block)])
      (unless (= i 9)
        (let ([str (car ls)])
          (unless (and (string? str) (= (string-length str) 9))
            (error 'input-block "invalid string ~s" str))
          (for-each 
            (lambda (c j)
              (grid-set! i j
                (case c
                  [(#\0) (make-vector 9 1)]
                  [(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                   (- (char->integer c) (char->integer #\0))]
                  [else (error 'input-block "invalid char ~s" c)])))
            (string->list str)
            '(0 1 2 3 4 5 6 7 8)))
        (f (add1 i) (cdr ls))))))

(define print-sudoku
  (lambda ()
    (printf "         column      ~%")
    (printf "    1 2 3 4 5 6 7 8 9~%")
    (newline)
    (let row-loop ([row 0])
      (unless (= row 9)
        (printf "~a   " (+ row 1))
        (let col-loop ([col 0])
          (if (= col 9)
              (begin
                (newline)
                (row-loop (+ row 1)))
              (begin
                (if (vector? (grid-ref row col))
                    (printf "  ")
                    (printf "~a " (grid-ref row col)))
                (col-loop (+ col 1)))))))))

(define sub-grid-list
  (lambda (sub)
    (case sub
      [(0) '(0 2 0 2)]
      [(1) '(0 2 3 5)]
      [(2) '(0 2 6 8)]
      [(3) '(3 5 0 2)]
      [(4) '(3 5 3 5)]
      [(5) '(3 5 6 8)]
      [(6) '(6 8 0 2)]
      [(7) '(6 8 3 5)]
      [(8) '(6 8 6 8)])))

(define sub-grid-list2
  (lambda (row col)
    (case row
      [(0 1 2)
       (case col
         [(0 1 2) 0]
         [(3 4 5) 1]
         [(6 7 8) 2])]
      [(3 4 5)
       (case col
         [(0 1 2) 3]
         [(3 4 5) 4]
         [(6 7 8) 5])]
      [(6 7 8)
       (case col
         [(0 1 2) 6]
         [(3 4 5) 7]
         [(6 7 8) 8])])))
      
(define row->vector
  (lambda (row)
    (let ([vec (make-vector 9 #f)])
      (let loop ([i 0])
        (if (= i 9)
            vec
            (begin
              (vector-set! vec i (grid-ref row i))
              (loop (+ i 1))))))))

(define row-init
  (lambda (row)
    (vector->row (vector-init (row->vector row)) row)))

(define row-init-all
  (lambda ()
    (let loop ([i 0])
      (unless (= i 9)
        (row-init i)
        (loop (+ i 1))))))

(define col->vector
  (lambda (col)
    (let ([vec (make-vector 9 #f)])
      (let loop ([i 0])
        (if (= i 9)
            vec
            (begin
              (vector-set! vec i (grid-ref i col))
              (loop (+ i 1))))))))

(define col-init
  (lambda (col)
    (vector->col (vector-init (col->vector col)) col)))

(define col-init-all
  (lambda () 
    (let loop ([i 0])
      (unless (= i 9)
        (col-init i)
        (loop (+ i 1))))))

(define sub-grid->vector
  (lambda (sub-grid)
    (let ([sub (sub-grid-list sub-grid)])
      (let ([vec (make-vector 9 #f)] [row1 (car sub)] [row2 (cadr sub)]
            [col1 (caddr sub)] [col2 (cadddr sub)])
        (let loop1 ([i row1] [k 0])
          (if (= k 9)
              vec
              (let loop2 ([j col1] [k k])
                (if (= j (+ col2 1))
                    (loop1 (+ i 1) k)
                    (begin
                      (vector-set! vec k (grid-ref i j))
                      (loop2 (+ j 1) (+ k 1)))))))))))    

(define sub-grid-init
  (lambda (sub)
    (vector->sub-grid (vector-init (sub-grid->vector sub)) sub)))

(define sub-grid-init-all
  (lambda ()
    (let loop ([i 0])
      (unless (= i 9)
        (sub-grid-init i)
        (loop (+ i 1))))))

(define vector->row
  (lambda (vec row)
    (let loop ([i 0])
      (unless (= i 9)
        (grid-set! row i (vector-ref vec i))
        (loop (+ i 1))))))

(define vector->col
  (lambda (vec col)
    (let loop ([i 0])
      (unless (= i 9)
        (grid-set! i col (vector-ref vec i))
        (loop (+ i 1))))))

(define vector->sub-grid
  (lambda (vec sub-grid)
    (let ([sub (sub-grid-list sub-grid)])
      (let ([row1 (car sub)] [row2 (cadr sub)]
            [col1 (caddr sub)] [col2 (cadddr sub)])
        (let loop1 ([i row1] [k 0])
          (unless (= k 9)
            (let loop2 ([j col1] [k k])
              (if (= j (+ col2 1))
                  (loop1 (+ i 1) k)
                  (begin
                    (grid-set! i j (vector-ref vec k))
                    (loop2 (+ j 1) (+ k 1)))))))))))

(define vector-init
  (lambda (vec)
    (let main-loop ([i 0])
      (if (= i 9)
          vec
          (let ([x (vector-ref vec i)])
            (if (vector? x)
                (main-loop (+ i 1))
                (let sub-loop ([j 0])
                  (if (= j 9)
                      (main-loop (+ i 1))
                      (let ([y (vector-ref vec j)])
                        (if (vector? y)
                            (begin
                              (vector-set! y (- x 1) 0)
                              (sub-loop (+ j 1)))
                            (sub-loop (+ j 1))))))))))))

(define grid-init
  (lambda ()
    (row-init-all)
    (col-init-all)
    (sub-grid-init-all)))

(define one-possible?
  (lambda (vec)
    (let loop ([i 0] [count 0])
      (if (= i 9)
          (= count 1)
          (if (= (vector-ref vec i) 1)
              (loop (+ i 1) (+ count 1))
              (loop (+ i 1) count))))))

(define replace-one
  (lambda (vec)
    (let loop ([i 0])
      (if (= (vector-ref vec i) 1)
          i
          (loop (+ i 1))))))

(define level-one-row
  (lambda (row)
    (let loop ([i 0] [count 0] [vec (row->vector row)])
      (if (= i 9)
          count
          (let ([x (vector-ref vec i)])
            (if (vector? x)
                (if (one-possible? x)
                    (begin
                      (grid-set! row i (+ (replace-one x) 1))
                      (row-init row)
                      (col-init i)
                      (sub-grid-init (sub-grid-list2 row i))
                      (loop (+ i 1) (+ count 1) (row->vector row)))
                    (loop (+ i 1) count vec))
                (loop (+ i 1) count vec)))))))

(define level-one-pass
  (lambda ()
    (let loop ([i 0] [count 0])
      (if (= i 9)
          count
          (loop (+ i 1) (+ count (level-one-row i)))))))
              
(define level-one
  (lambda ()
    (let loop ()
      (unless (= (level-one-pass) 0)
        (loop)))))

(define level-two-row
  (lambda (row)
    (let loop1 ([i 0] [count1 0] [vec (row->vector row)])
      (if (= i 9)
          count1
          (let loop2 ([j 0] [count2 0] [col 0])
            (if (= j 9)
                (if (= count2 1)
                    (begin
                      (grid-set! row col (+ i 1))
                      (row-init row)
                      (col-init col)
                      (sub-grid-init (sub-grid-list2 row col))
                      (loop1 (+ i 1) (+ count1 1) (row->vector row)))
                    (loop1 (+ i 1) count1 vec))
                (begin
                  (let ([x (vector-ref vec j)])
                    (if (vector? x)
                        (if (= (vector-ref x i) 1)
                            (loop2 (+ j 1) (+ count2 1) j)
                            (loop2 (+ j 1) count2 col))
                        (loop2 (+ j 1) count2 col))))))))))  

(define level-two-row-pass
  (lambda ()
    (let loop ([i 0] [count 0])
      (if (= i 9)
          count
          (loop (+ i 1) (+ count (level-two-row i)))))))

(define level-two-col
  (lambda (col)
    (let loop1 ([i 0] [count1 0] [vec (col->vector col)])
      (if (= i 9)
          count1
          (let loop2 ([j 0] [count2 0] [row 0])
            (if (= j 9)
                (if (= count2 1)
                    (begin
                      (grid-set! row col (+ i 1))
                      (row-init row)
                      (col-init col)
                      (sub-grid-init (sub-grid-list2 row col))                                           
                      (loop1 (+ i 1) (+ count1 1) (col->vector col)))
                    (loop1 (+ i 1) count1 vec))
                (begin
                  (let ([x (vector-ref vec j)])
                    (if (vector? x)
                        (if (= (vector-ref x i) 1)
                            (loop2 (+ j 1) (+ count2 1) j)
                            (loop2 (+ j 1) count2 row))
                        (loop2 (+ j 1) count2 row))))))))))  
 
(define level-two-col-pass
  (lambda ()
    (let loop ([i 0] [count 0])
      (if (= i 9)
          count
          (loop (+ i 1) (+ count (level-two-col i)))))))

(define level-two-sub
  (lambda (sub)
    (let loop1 ([i 0] [count1 0] [vec (sub-grid->vector sub)])
      (if (= i 9)
          count1
          (let loop2 ([j 0] [count2 0] [loc 0])
            (if (= j 9)
                (if (= count2 1)
                    (begin
                      (let ([row (+ (car (sub-grid-list sub)) (quotient loc 3))]
                            [col (+ (caddr (sub-grid-list sub))
                                    (remainder loc 3))])
                        (grid-set! row col (+ i 1))
                        (row-init row)
                        (col-init col)
                        (sub-grid-init sub))                     
                        (loop1 (+ i 1) (+ count1 1) (sub-grid->vector sub)))
                    (loop1 (+ i 1) count1 vec))
                (begin
                  (let ([x (vector-ref vec j)])
                    (if (vector? x)
                        (if (= (vector-ref x i) 1)
                            (loop2 (+ j 1) (+ count2 1) j)
                            (loop2 (+ j 1) count2 loc))
                        (loop2 (+ j 1) count2 loc))))))))))  

(define level-two-sub-pass
  (lambda ()
    (let loop ([i 0] [count 0])
      (if (= i 9)
          count
          (loop (+ i 1) (+ count (level-two-sub i)))))))

(define vector-count
  (lambda (vec)
    (let loop ([i 0] [count 0])
      (if (= i 9)
          count
          (if (= (vector-ref vec i) 1)
              (loop (+ i 1) (+ count 1))
              (loop (+ i 1) count))))))

(define replace-2
  (lambda (vec loc1 loc2)
    (let loop1 ([i 0] [count1 0])
      (if (= i 9)
          (list count1 vec)
          (let ([x (vector-ref vec i)])
            (if (and (vector? x) (not (= loc1 i)) (not (= loc2 i)))
                (let loop2 ([j 0] [count2 count1])
                  (if (= j 9)
                      (loop1 (+ i 1) count2)
                      (let ([y (vector-ref vec loc1)])
                        (if (and (= (vector-ref y j) 1) (= (vector-ref x j) 1))
                            (begin
                              (vector-set! (vector-ref vec i) j 0)
                              (loop2 (+ j 1) (+ count2 1)))
                            (loop2 (+ j 1) count2)))))                         
                (loop1 (+ i 1) count1)))))))

(define level-three-col
  (lambda (col)
    (let loop1 ([i 0] [count1 0] [vec (col->vector col)])
      (if (= i 9)
          count1
          (let ([x1 (vector-ref vec i)])
            (if (vector? x1)
                (let loop2 ([j 0])
                  (if (= j 9)
                      (loop1 (+ i 1) count1 vec)
                      (let ([x2 (vector-ref vec j)])
                        (if (and (vector? x2) (not (= i j)) (equal? x1 x2))
                            (let ([y1 (vector-count x1)])
                              (case y1
                                [(2) (begin
                                       (let ([z (replace-2 vec i j)])
                                         (if (= (car z) 0)
                                             (loop2 (+ j 1))
                                             (begin
                                               (vector->col (cadr z) col)
                                               (loop1 (+ i 1) (+ count1 (car z)) (col->vector col))))))]
                                [(3) (loop2 (+ j 1))]
                                [(else) (loop2 (+ j 1))]))                             
                              (loop2 (+ j 1))))))
                  (loop1 (+ i 1) count1 vec)))))))
                      

(define done?
  (lambda ()
    (let row-loop ([row 0] [count 0])
      (if (= row 9)
          (= count 0)
          (let col-loop ([col 0] [count count])
            (if (= col 9)
                (row-loop (+ row 1) count)
                (let ([x (grid-ref row col)])
                  (if (vector? x)
                      (col-loop (+ col 1) (+ count 1))
                      (col-loop (+ col 1) count)))))))))

(define invalid?
  (lambda ()
    (let row-loop ([row 0] [count 0])
      (if (= row 9)
          (> count 0)
          (let col-loop ([col 0] [count count])
            (if (= col 9)
                (row-loop (+ row 1) count)
                (let ([x (grid-ref row col)])
                  (if (and (vector? x) (= (vector-count x) 0))
                      (col-loop (+ col 1) (+ count 1))
                      (col-loop (+ col 1) count)))))))))

(define copy-grid
  (lambda ()
    (let ([grid-orig (make-grid)])
      (let row-loop ([row 0])
        (if (= row 9)
            grid-orig
            (let col-loop ([col 0])
              (if (= col 9)
                  (row-loop (+ row 1))
                  (let ([x1 (table-ref grid row col)] [y1 (table-ref grid-orig col row)])
                    (if (not (vector? x1))
                        (begin
                          (table-set! grid-orig col row x1)
                          (col-loop (+ col 1)))
                        (let vector-loop ([loc 0])
                          (if (= loc 9)
                              (col-loop (+ col 1))
                              (let ([x2 (vector-ref x1 loc)] [y2 (vector-ref y1 loc)])
                                (vector-set! y1 loc x2)
                                (vector-loop (+ loc 1))))))))))))))

(define restore-grid
  (lambda (grid-orig)
    (reset-grid)
    (let row-loop ([row 0])
      (unless (= row 9)
        (let col-loop ([col 0])
          (if (= col 9)
              (row-loop (+ row 1))
              (let ([x1 (table-ref grid row col)] [y1 (table-ref grid-orig row col)])
                (if (not (vector? y1))
                    (begin
                      (grid-set! row col y1)
                      (col-loop (+ col 1)))
                    (let vector-loop ([loc 0])
                      (if (= loc 9)
                          (col-loop (+ col 1))
                          (let ([y2 (vector-ref y1 loc)])
                            (vector-set! (grid-ref row col) loc y2)
                            (vector-loop (+ loc 1)))))))))))))
                               

(define level-four
  (lambda ()
    (let row-loop ([row 0])
      (unless (= row 9)
        (let col-loop ([col 0])
          (if (= col 9)
              (row-loop (+ row 1))
              (let ([x (grid-ref row col)])
                (if (and (vector? x) (= (vector-count x) 2))
                    (let vector-loop ([loc 0])
                      (if (= loc 9)
                          (col-loop (+ col 1))
                          (let ([x2 (grid-ref row col)])
                            (if (= (vector-ref x2 loc) 1)
                                (let ([grid-orig (copy-grid)])
                                  (vector-set! (grid-ref row col) loc 0)
                                  (if (sudoku-help grid)
                                      #t
                                      (begin
                                        (restore-grid grid-orig)
                                        (vector-loop (+ loc 1)))))
                                (vector-loop (+ loc 1))))))
                    (col-loop (+ col 1))))))))))
                        
(define sudoku-help
  (lambda (grid)
    (grid-init)
    (let loop ()
      (cond
        [(and (number? (level-one-pass)) (> (level-one-pass) 0)) (loop)]
        [(invalid?) #f]
        [(and (number? (level-two-row-pass)) (> (level-two-row-pass) 0)) (loop)]
        [(and (number? (level-two-col-pass)) (> (level-two-col-pass) 0)) (loop)]
        [(and (number? (level-two-sub-pass)) (> (level-two-sub-pass) 0)) (loop)]
        [(done?) #t]
        [else (level-four)]))))

(define sudoku
  (lambda (block)
  (define grid (make-grid))
  (input-block block)
  (if (sudoku-help grid)
      (print-sudoku)
      (error 'sudoku "MIS-ENTERED INITIALIZATION"))))

(define do-file
  (lambda ()
    (let f ()
      (let ([x (read)])
        (unless (eof-object? x)
           (sudoku x)
           (f))))))
(with-input-from-file "sudoku.txt" do-file)
(with-input-from-file "sudoku-hard.txt" do-file)

(exit)
;;; vim:syntax=scheme
