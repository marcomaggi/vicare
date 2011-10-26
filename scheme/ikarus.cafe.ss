;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
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

#|procedure:new-cafe
synopsis:
  (new-cafe [eval])
description:
  The procedure new-cafe starts a new read-eval-print loop inside
  the current cafe (if one exists).  It prompts the user for an
  expression, evaluates it, prints the result back, and repeats the
  process.  If new-cafe is called with an argument, eval, then that
  argument must be a procedure that takes a single argument.  The
  eval procedure will be used to evaluate the expressions.

  Every time a new cafe is started, the prompt is changed to reflect
  the depth of the current cafe (i.e. how many eof objects is takes
  to exit the outermost cafe).

  Input and output performed by the cafe can be changed by the
  console-input-port and console-output-port parameters.

  If an die occurs during reading, evaluating, or printing an
  expression, then the die is printed to the error-port and the
  operations of the cafe resume as normal.|#
#|FIXME:new-cafe
  Be specific about what the error-port is |#

(library (ikarus cafe)
  (export new-cafe waiter-prompt-string)
  (import
    (only (rnrs) with-exception-handler)
    (except (ikarus) new-cafe waiter-prompt-string))

  (define eval-depth 0)

  (define waiter-prompt-string
    (make-parameter "vicare>"
      (lambda (x)
        (if (string? x)
            x
            (die 'waiter-prompt-string "not a string" x)))))

  (define display-prompt
    (lambda (i)
      (if (fx= i eval-depth)
          (display " " (console-output-port))
          (begin
            (display (waiter-prompt-string) (console-output-port))
            (display-prompt (fx+ i 1))))))

  (define (print-ex ex)
    (flush-output-port (console-output-port))
    (display "Unhandled exception\n" (console-error-port))
    (print-condition ex (console-error-port)))

  (define (reset k)
    (reset-input-port! (console-input-port))
    (k))

  (define wait1
    (lambda (eval-proc k escape-k)
      (display-prompt 0)
      (let ([x (with-exception-handler
                 (lambda (ex)
                   (cond
                     [(lexical-violation? ex)
                      (print-ex ex)
                      (reset k)]
                     [(interrupted-condition? ex)
                      (flush-output-port (console-output-port))
                      (newline (console-output-port))
                      (reset k)]
                     [else (raise-continuable ex)]))
                 (lambda ()
                   (read (console-input-port))))])
        (cond
          [(eof-object? x)
           (newline (console-output-port))
           (escape-k (void))]
          [else
           (call-with-values
             (lambda ()
               (with-exception-handler
                 (lambda (ex)
                   (if (non-continuable-violation? ex)
                       (reset k)
                       (raise-continuable ex)))
                 (lambda ()
                   (with-exception-handler
                     (lambda (ex)
                       (print-ex ex)
                       (when (serious-condition? ex)
                         (reset k)))
                     (lambda ()
                       (eval-proc x))))))
             (lambda v*
               (unless (andmap (lambda (v) (eq? v (void))) v*)
                 (with-exception-handler
                   (lambda (ex)
                     (cond
                       [(interrupted-condition? ex)
                        (flush-output-port (console-output-port))
                        (newline (console-output-port))
                        (reset k)]
                       [else (raise-continuable ex)]))
                   (lambda ()
                     (for-each
                       (lambda (v)
                         (pretty-print v (console-output-port)))
                       v*))))))]))))

  (define do-new-cafe
    (lambda (eval-proc)
      (dynamic-wind
        (lambda () (set! eval-depth (fxadd1 eval-depth)))
        (lambda ()
          (call/cc
            (lambda (k)
              (let loop ()
                (call/cc
                  (lambda (k1)
                    (with-exception-handler
                      (lambda (ex)
                        (with-exception-handler k1
                          (lambda ()
                            (flush-output-port (console-output-port))
                            (newline (console-output-port))
                            (reset k1))))
                      (lambda () (wait1 eval-proc k1 k)))))
                (loop)))))
        (lambda () (set! eval-depth (fxsub1 eval-depth))))))

  (define default-cafe-eval
    (lambda (x)
      (eval x (interaction-environment))))

  (define new-cafe
    (case-lambda
      [() (do-new-cafe default-cafe-eval)]
      [(p)
       (unless (procedure? p)
         (die 'new-cafe "not a procedure" p))
       (do-new-cafe p)]))
  )

