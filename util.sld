(define-library (util)
  (export symbol-append)
  (import (scheme base))
  (begin
    (define (symbol-append a b)
      (string->symbol (string-append (symbol->string a) (symbol->string b))))))
