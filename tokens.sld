(define-library (tokens)
  (export tokenize string-split)
  (import (scheme base))
  (begin
    (define (tokenize str)
      (string-split str #\space))

    ; return a list of strings
    ; str: string to split
    ; sep: character to split on
    (define (string-split str sep)
      (let ((len (string-length str)))
        (string-split-impl str sep len (- len 1) '())))

    ; return a list of strings
    ; str: string to split
    ; sep: character to split on
    ; word-end: index of the end of the current word
    ; current-index: index of the current character (working backwards)
    ; result: list of separated strings
    (define (string-split-impl str sep word-end current-index result)
      (if (> current-index 0)
        (let ((char (string-ref str current-index)))
          (if (eqv? sep char)
            (string-split-impl str sep current-index
                                      (- current-index 1)
                                      (cons (substring str (+ 1 current-index) word-end) result))
            (string-split-impl str sep word-end (- current-index 1) result)))
        (cons (substring str current-index word-end) result))))) ; add the first word
