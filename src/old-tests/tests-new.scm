
(add-tests-with-string-output "allocating procedures live-across a call"
  [(let ([g 
          (lambda (y)
            (let ([f (lambda (x) y)])
               (map f '(1 2 3))
               (map f '(1 2 3))))])
     (g 2)) => "(2 2 2)\n"]
  [(let ()
     (define (mklist i ac)
       (cond
         [(#%$fxzero?  i) ac]
         [else (mklist (#%$fxsub1 i) (#%cons i ac))]))
     (define (leng ls n)
       (cond
         [(null? ls) n]
         [else (leng (#%$cdr ls) (#%$fxadd1 n))]))
     (leng (mklist 10000000 '()) 0)) => "10000000\n"]
  )

    
