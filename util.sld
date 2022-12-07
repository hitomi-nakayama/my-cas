(define-library (util)
  (export char->symbol symbol-append)
  (import (scheme base))
  (begin
    (define (char->symbol c)
      (string->symbol (string c)))
    (define (symbol-append a b)
      (string->symbol (string-append (symbol->string a) (symbol->string b))))))
