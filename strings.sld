(define-library (strings)
  (export concat)
  (import (scheme base)
          (scheme write)
          (srfi 6))
  (begin
    ; concatinate values into a string
    (define (concat . args)
      (let ((output (open-output-string)))
        (for-each
          (lambda (x)
            (display x output))
          args)
        (get-output-string output)))))
