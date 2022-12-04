(define-library (table)
  (export table <table> table-ref table-find-pair table-keys)
  (import (scheme base)
          (srfi 1))
  (begin
    ; simple association table / dictionary
    ; O(n) lookup
    ;
    ; (table <key-eq> ((<key>) <value>) ...)
    ; (table ((<key>) <value>) ...)
    ; key-eq is a function that takes two keys and returns #t if they are equal
    ;
    ; Example usage
    ; (table () (('a) 12)
    ;        (('b) 34))
    ; Usage with string keys:
    ; (table (string=?) (("X") 100)
    ;                   (("Y") 200))
    (define-syntax table
      (syntax-rules ()
        ((_ () ((a) b) ...)
          (let ((data (list (cons a b) ...)))
            (table-record data eqv?)))
        ((_ (key-eq) ((a) b) ...)
          (let ((data (list (cons a b) ...)))
            (table-record data key-eq)))))

    (define-record-type <table>
      (table-record key-values key-eq)
      table?
      (key-values table-key-values)
      (key-eq table-key-eq))

    (define (table-ref t k)
      (let ((pair (table-find-pair t k)))
        (if pair
            (cdr pair)
            (error "table-ref: no such key" k))))

    (define (table-find-pair t k)
      (assoc k (table-key-values t) (table-key-eq t)))

    (define (table-keys t)
      (map car (table-key-values t)))))
