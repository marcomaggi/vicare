#!/usr/bin/env ikarus --r6rs-script

(import (ikarus) (objc) (Cocoa helpers))

(define-class NSSpeechSynthesizer)

(define (get-voice-name)
  (if (= 2 (length (command-line)))
      (cadr (command-line))
      #f))

(define (make-speaker voice)
  (define base-string "com.apple.speech.synthesis.voice.")
  (define (synthesizer x)
    [$ [$ NSSpeechSynthesizer alloc]
       initWithVoice: 
         (and x (nsstring (string-append base-string x)))])
  (define (voice->synthesizer voice) 
    (or (synthesizer voice)
        (begin
          (printf "~a is not available\n" voice)
          (synthesizer #f))
        (error #f "cannot initialize voice")))
  (let ([st (voice->synthesizer voice)])
    (lambda (x)
      [$ st startSpeakingString: (nsstring x)])))

(define speak (make-speaker (get-voice-name)))

(speak "may I help you?")
(let loop ()
  (printf "> ")
  (let ([x (get-line (current-input-port))])
    (unless (eof-object? x)
      (speak x)
      (loop))))
(newline)

