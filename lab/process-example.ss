#!/usr/bin/env scheme-script

(import (ikarus))

(let-values ([(pid in out err) (process "date")])
  (printf "pid=~s\n" pid)
  (let f ()
    (let ([x (get-u8 out)])
      (unless (eof-object? x) 
        (write-char (integer->char x))
        (f))))
  (flush-output-port)
  (close-output-port in)
  (close-input-port out)
  (close-input-port err)
  (printf "exit status = ~s\n" (waitpid pid)))
