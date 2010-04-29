
(source "r6rs.ss")
(source "parameters.ss")
(source "match.ss")
(source "interp.ss")

(library main
  (export)
  (import interp r6rs)
  (write (ee '(let ((x 5)) 
                (let ((y (+ x x)))
                  (+ y x)))))
  (newline))

(invoke main)
;(dump main "main.pp")
