(module ()
  #| set the error handler to output an error page should
     an error occur |#
  (error-handler
    (lambda (who msg . args)
      (display "Content-type: text/html\n\n")
      (display "<font color=\"red\"><pre>")
      (if who
          (printf "Error: ~a.\n" (apply format msg args))
          (printf "Error in ~a: ~a.\n" who (apply format msg args)))
      (display "</pre></font>")
      (exit 0))))
