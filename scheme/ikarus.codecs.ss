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


(library (ikarus codecs)
  (export latin-1-codec utf-8-codec utf-16-codec native-eol-style
          make-transcoder native-transcoder buffer-mode?
          transcoder-codec transcoder-eol-style
          transcoder-error-handling-mode)
  (import 
    (except (ikarus) latin-1-codec utf-8-codec utf-16-codec 
      native-eol-style make-transcoder native-transcoder
      buffer-mode? transcoder-codec
      transcoder-eol-style transcoder-error-handling-mode)
    (ikarus system $transcoders))
  (define (latin-1-codec) 'latin-1-codec)
  (define (utf-8-codec)   'utf-8-codec)
  (define (utf-16-codec)  'utf-16-codec)
  (define (native-eol-style) 'none)
  
  (define error-handling-mode-alist
    '([ignore .  #b01]
      [raise .   #b10]
      [replace . #b11]))
  (define error-handling-mode-mask #b11)

  (define eol-style-alist
    '([none .   #b00000]
      [lf .     #b00100]
      [cr .     #b01000]
      [crlf .   #b01100]
      [nel .    #b10000]
      [crnel .  #b10100]
      [ls .     #b11000]))
  (define eol-style-mask #b11100)

  (define codec-alist
    '([latin-1-codec . #b0100000]
      [utf-8-codec .   #b1000000]
      [utf-16-codec .  #b1100000]))
  (define codec-mask #b11100000)

  (define (rev-lookup n ls)
    (cond
      [(null? ls) #f]
      [(= (cdar ls) n) (caar ls)]
      [else (rev-lookup n (cdr ls))]))

  (define (codec->fixnum x who)
    (cond
      [(assq x codec-alist) => cdr]
      [else (die who "not a valid coded" x)]))

  (define (eol-style->fixnum x who)
    (cond
      [(assq x eol-style-alist) => cdr]
      [else (die who "not a valid eol-style" x)]))

  (define (error-handling-mode->fixnum x who)
    (cond
      [(assq x error-handling-mode-alist) => cdr]
      [else (die who "not a valid error-handling mode" x)]))

  (define make-transcoder
    (case-lambda
      [(codec eol-style handling-mode) 
       ($data->transcoder 
         (fxior 
           (error-handling-mode->fixnum handling-mode 'make-transcoder)
           (eol-style->fixnum eol-style 'make-transcoder)
           (codec->fixnum codec 'make-transcoder)))]
      [(codec eol-style) 
       (make-transcoder codec eol-style 'replace)]
      [(codec) 
       (make-transcoder codec 'none 'replace)]))

  (define (native-transcoder) 
    (make-transcoder 'utf-8-codec 'none 'replace))

  (define (transcoder-codec x) 
    (define who 'transcoder-codec)
    (if (transcoder? x) 
        (let ([tag (fxlogand ($transcoder->data x) codec-mask)])
          (or (rev-lookup tag codec-alist)
              (die who "transcoder has no codec" x)))
        (die who "not a transcoder" x)))

  (define (transcoder-eol-style x) 
    (define who 'transcoder-eol-style)
    (if (transcoder? x) 
        (let ([tag (fxlogand ($transcoder->data x) eol-style-mask)])
          (or (rev-lookup tag eol-style-alist)
              (die who "transcoder has no eol-style" x)))
        (die who "not a transcoder" x)))

  (define (transcoder-error-handling-mode x) 
    (define who 'transcoder-error-handling-mode)
    (if (transcoder? x) 
        (let ([tag (fxlogand ($transcoder->data x) error-handling-mode-mask)])
          (or (rev-lookup tag error-handling-mode-alist)
              (die who "transcoder has no error-handling mode" x)))
        (die who "not a transcoder" x)))

  (define (buffer-mode? x)
    (and (memq x '(none line block)) #t))

  )

