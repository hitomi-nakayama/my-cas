(import (scheme base)
        (scheme write)
        (scheme process-context)
        (tokens))

(define (main)
  (if (eqv? 2 (length (command-line)))
      (tests)
      (repl)))

(define (repl)
  (define (loop)
    (display "> ")
    (let ((line (read-line)))
      (if (not (string=? line "quit"))
        (begin
          (let ((tokens (tokenize line)))
            (print tokens))
          (loop)))))
  (loop))

(define (print . args)
  (for-each
    (lambda (x)
      (display x)
      (display " "))
    args)
  (newline))

(main)
