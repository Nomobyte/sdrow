(import (chicken format)
        (chicken string)
        (chicken io)
        (chicken random)
        (chicken process-context)
        getopt-long)

;; file

(define (read-file-lines file #!optional max)
  (call-with-input-file file
    (lambda (port) (read-lines port max))))

;; math

(define (1+ x) (+ x 1))

(define (truncate-integer->even number)
  (- number (remainder number 2)))
(define (truncate-integer->odd number)
  (- (1+ number) (remainder number 2)))

;; string

(define (string-map proc #!rest strings)
  (let* ((min-length (apply min (map string-length strings)))
         (str (make-string min-length)))
    (do ((i 0 (1+ i)))
        ((>= i min-length) str)
      (string-set! str i (apply proc (map (lambda (str) (string-ref str i))
                                          strings))))))

;; command-line

(define (option-ref options key #!optional (default-value #f))
  (cond ((assq key options) => cdr)
        (else default-value)))
(define (other-options options) (option-ref options '@ #f))

(define grammar
  `((case-sensitive "Case sensitive comparison if it is specified; otherwise, case insensitive."
                    (single-char #\s)
                    (value #f))
    (hint "Display a hint."
          (single-char #\i)
          (value #f))
    (hint-char "The default character for a hint."
               (single-char #\c)
               (value #t)
               (value (required CHAR)
                      (predicate ,(lambda (str) (= 1 (string-length str))))
                      (transformer ,(lambda (str) (string-ref str 0)))))
    (description "Display a description of a word everytime; otherwise, it will only be displayed once if it need."
                 (single-char #\d)
                 (value #f))
    (mode "The program's mode. Possible values are list, alist, ralist."
      (single-char #\m)
      (value (required MODE)
             (predicate ,(lambda (x) (member x '("list" "alist" "ralist"))))
             (transformer ,string->symbol)))
    (max-errors "Maximum number of errors allowed. It is 2^64 if it isn't specified."
                (single-char #\e)
                (value #t)
                (value (required VALUE)
                       (predicate ,string->number)
                       (transformer ,string->number)))
    (max-loops "Maximum number of loops allowed. It is 2^64 if it isn't specified."
               (single-char #\l)
               (value #t)
               (value (required VALUE)
                      (predicate ,string->number)
                      (transformer ,string->number)))                        
    (hide-answer "Hide an answer."
                 (single-char #\a)
                 (value #f))
    (help "Display this help message and exit."
          (single-char #\h)
          (value #f))))

(define (display-help-message)
  (format #t "Usage: sdrow [OPTION]... FILE
The program like Anki and Wordle.

~A" (usage grammar)))

(define (display-missing-operand-message)
  (format #t "~A~%"
          "sdrow: missing operand
Try 'sdrow --help' for more information."))

;; main logic

(define (game-loop word-vector #!key hint-wanted hint-char description-wanted mode max-errors max-loops hide-answer case-sensitive)
  (define (guess->hint correct-string guess-string)
    (let ((comp-proc (if case-sensitive char=? char-ci=?)))
      (string-map (lambda (cc gc) (if (comp-proc cc gc) cc hint-char)) correct-string guess-string)))
  (define (read-guess)
    (display "@> ") (read-line))
  (define (display-description description)
    (when description (format #t "~A~%" description)))
  (define (display-hint hint)
    (format #t "?> ~A~%" hint))
  (define (display-answer answer)
    (format #t "~%---~A---~%" answer))
  (define (empty-hint word)
    (string-map (lambda (x) hint-char) word))
  (define (next-entry-number)
    (pseudo-random-integer (vector-length word-vector)))
  (define entry-number (next-entry-number))
  (define (read-word)
    (case mode
      ((list) (vector-ref word-vector entry-number))
      ((alist) (vector-ref word-vector (truncate-integer->odd entry-number)))
      ((ralist) (vector-ref word-vector (truncate-integer->even entry-number)))))
  (define (read-description)
    (case mode
      ((list) #f)
      ((alist) (vector-ref word-vector (truncate-integer->even entry-number)))
      ((ralist) (vector-ref word-vector (truncate-integer->odd entry-number)))))
  (define (guess-loop correct-word)
    (let ((comp-proc (if case-sensitive string=? string-ci=?)))
      (do ((error 0 (1+ error))
           (guess (read-guess) (read-guess)))
          ((or (>= error max-errors)
               (comp-proc correct-word guess))
           (when (not hide-answer) (display-answer correct-word)))
        (when description-wanted (display-description description))
        (when hint-wanted (display-hint (guess->hint correct-word guess))))))
  
  (do ((loop 0 (1+ loop))
       (word (read-word) (read-word))
       (description (read-description) (read-description)))
      ((>= loop max-loops))
    (set! entry-number (next-entry-number))
    (display-description description)
    (when hint-wanted (display-hint (empty-hint word)))
    (guess-loop word)
    (newline)))

(define (main args)
  (set-pseudo-random-seed! (apply string-append (read-file-lines "/dev/urandom" 16)))
  (let ((options (getopt-long args grammar)))
    (cond ((option-ref options 'help #f) (display-help-message))
          ((null? (other-options options)) (display-missing-operand-message))
          (else (game-loop (list->vector (read-file-lines (car (other-options options))))
                           #:hint-wanted (option-ref options 'hint #f)
                           #:hint-char (option-ref options 'hint-char #\_)
                           #:description-wanted (option-ref options 'description #f)
                           #:mode (option-ref options 'mode 'list)
                           #:max-errors (option-ref options 'max-errors (expt 2 64))
                           #:max-loops (option-ref options 'max-loops (expt 2 64))
                           #:hide-answer (option-ref options 'hide-answer #f)
                           #:case-sensitive (option-ref options 'case-sensitive #f)
                           )))))
