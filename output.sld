(define-library (output)
  (export print)
  (import (scheme base)
          (scheme write))
  (begin
    (define (print . args)
      (for-each
        (lambda (x)
          (display x)
          (display " "))
        args)
      (newline))))
