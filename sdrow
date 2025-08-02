#! /usr/bin/guile \
-e main -s
!#
(use-modules (ice-9 textual-ports)
             (ice-9 getopt-long)
             (srfi srfi-1))

(define (get-lines input-port)
  (drop-right! (string-split (get-string-all input-port) #\linefeed) 1))
(define (get-file-lines file)
  (call-with-input-file file
    (lambda (port) (get-lines port))))

(define (truncate-integer->even number)
  (- number (remainder number 2)))
(define (truncate-integer->odd number)
  (- (1+ number) (remainder number 2)))

(define* (string-map+ proc #:rest strings)
  (let* ((min-length (apply min (map string-length strings)))
         (str (make-string min-length)))
    (do ((i 0 (1+ i)))
        ((>= i min-length) str)
      (string-set! str i (apply proc (map (lambda (str) (string-ref str i))
                                          strings))))))

(define (other-options options) (option-ref options '() #f))

(define* (game-loop word-vector #:key hint-wanted hint-char description-wanted mode max-errors max-loops hide-answer case-sensitive)
  (define* (guess->hint correct-string guess-string)
    (let ((comp-proc (if case-sensitive char=? char-ci=?)))
      (string-map+ (lambda (cc gc) (if (comp-proc cc gc) cc hint-char)) correct-string guess-string)))
  (define (get-guess) (display "@> ") (get-line (current-input-port)))
  (define (display-description description) (when description (format #t "~A~%" description)))
  (define (display-hint hint) (format #t "?> ~A~%" hint))
  (define (display-answer answer) (format #t "~%---~A---~%" answer))
  (define (empty-hint word) (string-map (lambda (x) hint-char) word))
  (define (next-entry-number) (random (vector-length word-vector)))
  (define entry-number (next-entry-number))
  (define (get-word)
    (case mode
      ((list) (vector-ref word-vector entry-number))
      ((alist) (vector-ref word-vector (truncate-integer->odd entry-number)))
      ((ralist) (vector-ref word-vector (truncate-integer->even entry-number)))))
  (define (get-description)
    (case mode
      ((list) #f)
      ((alist) (vector-ref word-vector (truncate-integer->even entry-number)))
      ((ralist) (vector-ref word-vector (truncate-integer->odd entry-number)))))
  (define (guess-loop correct-word)
    (let ((comp-proc (if case-sensitive string=? string-ci=?)))
      (do ((error 0 (1+ error))
           (guess (get-guess) (get-guess)))
          ((or (>= error max-errors)
               (comp-proc correct-word guess))
           (when (not hide-answer) (display-answer correct-word)))
        (when description-wanted (display-description description))
        (when hint-wanted (display-hint (guess->hint correct-word guess))))))
  
  (do ((loop 0 (1+ loop))
       (word (get-word) (get-word))
       (description (get-description) (get-description)))
      ((>= loop max-loops))
    (set! entry-number (next-entry-number))
    (display-description description)
    (when hint-wanted (display-hint (empty-hint word)))
    (guess-loop word)
    (newline)))

(define help-message
  "Usage: sdrow [OPTION]... FILE
The program like Anki and Wordle.

  -r, --random            Enable random.
  -s, --case-sensitive    Case sensitive comparison if it is specified; otherwise, case insensitive.
  -i, --hint              Display a hint.
  -c, --hint-char VALUE   The default character for a hint.
  -d, --description       Display a description of a word everytime; otherwise, it will only be displayed once if it need.
  -m, --mode VALUE        The program's mode. Possible values are list, alist, ralist.
  -e, --max-errors VALUE  Maximum number of errors allowed. It is infinity if it isn't specified.
  -l, --max-loops VALUE   Maximum number of loops allowed. It is infinity if it isn't specified.
  -a, --hide-answer       Hide an answer.
  -h, --help              Display this help message and exit.")

(define missing-operand-message
  "sdrow: missing operand
Try 'sdrow --help' for more information.")

(define (main args)
  (let* ((option-spec '((random (single-char #\r) (value #f))
                        (case-sensitive (single-char #\s) (value #f))
                        (hint (single-char #\i) (value #f))
                        (hint-char (single-char #\c) (value #t))
                        (mode (single-char #\m) (value #t))
                        (max-errors (single-char #\e) (value #t))
                        (max-loops (single-char #\l) (value #t))
                        (description (single-char #\d) (value #f))
                        (hide-answer (single-char #\a) (value #f))
                        (help (single-char #\h) (value #f))))
         (options (getopt-long args option-spec)))
    (cond ((option-ref options 'help #f)
           (format #t "~A~%" help-message))
          ((null? (other-options options))
           (format #t "~A~%" missing-operand-message))
          (else (when (option-ref options 'random #f)
                  (set! *random-state* (random-state-from-platform)))
                (game-loop (list->vector (get-file-lines (car (other-options options))))
                           #:hint-wanted (option-ref options 'hint #f)
                           #:hint-char (string-ref (option-ref options 'hint-char "_") 0)
                           #:description-wanted (option-ref options 'description #f)
                           #:mode (string->symbol (option-ref options 'mode "list"))
                           #:max-errors (string->number (option-ref options 'max-errors "+inf.0"))
                           #:max-loops (string->number (option-ref options 'max-loops "+inf.0"))
                           #:hide-answer (option-ref options 'hide-answer #f)
                           #:case-sensitive (option-ref options 'case-sensitive #f)
                           )))))
