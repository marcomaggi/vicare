

(library interp
  (export ee)
  (import match r6rs)
  (define (constant? x)
    (or (number? x) (char? x) (string? x) (boolean? x)))
  (define (ee x)
    (define (ee x env)
      (match x
        (,c (guard (constant? c)) c)
        (,x (guard (symbol? x))
         (cond
           ((assq x env) => cdr)
           (else (error 'ee "unbound ~s" x))))
        ((let ((,x ,v)) ,b)
         (ee b (cons (cons x (ee v env)) env)))
        ((+ ,(x) ,(y)) (+ x y))
        (,others (error 'ee "invalid expr ~s" others))))
    (ee x '())))

