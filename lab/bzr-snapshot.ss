(import (ikarus))

(define (shell cmd . args)
  (let-values ([(pid op ip ep) (apply process cmd args)])
    (let ([ip (transcoded-port ip (native-transcoder))])
      (let ([str (get-string-all ip)])
        (close-input-port ip)
        (close-output-port op)
        (close-input-port ep)
        (let ([w (waitpid pid)])
          (unless (zero? (wstatus-exit-status w))
            (apply error 'shell "failed" w cmd args)))
        str))))

(define (make-snapshot repo dest-dir file-template)
  (let ([revno 
         (read (open-string-input-port (shell "bzr" "revno" repo)))])
    (let ([file (string-append dest-dir "/" (format file-template revno))])
      (unless (file-exists? file)
        (printf "extracting revision ~s to ~s\n" revno file)
        (shell "bzr" "export"
               "-r" (number->string revno) 
               file repo)))))

;(let ()
;  (define home (getenv "HOME"))
;  (define repo (string-append home "/Work/" "ikarus.dev"))
;  (define dest-dir (string-append repo "/snapshots"))
;  (define file-template "ikarus-scheme-r~a.tgz")
;  (make-snapshot repo dest-dir file-template))

(apply
  (case-lambda
    [(script repo dest-dir file-template)
     (make-snapshot repo dest-dir file-template)]
    [(script . args)
     (error script (format "Usage: ~a <repo-dir> <dest-dir> <file-template>" script))])
  (command-line))
