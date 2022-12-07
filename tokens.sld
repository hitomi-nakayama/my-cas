(define-library (tokens)
  (export tokenize string-split)
  (import (scheme base)
          (srfi 26)
          (constants))
  (begin
    (define (tokenize str)
      (let* ((words (string-split str (cut eqv? #\space <>))))
        (map string->obj words)))

    (define (string->obj str)
      (if (or (string=? str ")") (string=? str "("))
        (string-ref str 0)
        (or (string->number str)
            (string->symbol str))))

    ; return a list of strings
    ; str: string to split
    ; sep?: return true if it is a character so split on
    (define (string-split str sep?)
      (let ((len (string-length str)))
        (string-split-impl str sep? len (- len 1) '())))

    ; return a list of strings
    ; str: string to split
    ; sep: character to split on
    ; word-end: index of the end of the current word
    ; current-index: index of the current character (working backwards)
    ; result: list of separated strings
    (define (string-split-impl str sep? word-end current-index result)
      (if (> current-index 0)
        (let ((char (string-ref str current-index)))
          (if (sep? char)
            (string-split-impl str sep? current-index
                                      (- current-index 1)
                                      (cons (substring str current-index word-end) result))
            (string-split-impl str sep? word-end (- current-index 1) result)))
        (cons (substring str current-index word-end) result))))) ; add the first word
