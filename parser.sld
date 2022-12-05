(define-library (parser)
  (export operator? shunting-yard)
  (import (scheme base)
          (scheme write)
          (srfi 1)
          (output)
          (table))
  (begin
    (define (shunting-yard tokens)
      (shunting-yard-impl tokens '() '()))

    (define (shunting-yard-impl tokens operator-stack output)
      (if (not (null? tokens))
        (let ((lookahead (car tokens)))
          (cond
            ((null? tokens) output)
            ((number? lookahead) (shunting-yard-impl (cdr tokens)
                                                    operator-stack
                                                    (cons lookahead output)))
            ((operator? lookahead)
              (let-values (((popped-operators rest-of-stack) (span
                                                                (lambda (x)
                                                                  (and
                                                                    (operator? x)
                                                                    (or
                                                                      (and
                                                                        (> (precedence x)
                                                                          (precedence lookahead)))
                                                                        (and (left-associative? x)
                                                                          (= (precedence x)
                                                                            (precedence lookahead))))))
                                                                operator-stack)))
                (shunting-yard-impl (cdr tokens)
                                    (cons lookahead rest-of-stack)
                                    (append (reverse popped-operators) output))))
            ((eq? lookahead lparen) (shunting-yard-impl (cdr tokens)
                                                        (cons lookahead operator-stack)
                                                        output))
            ((eq? lookahead rparen)
              (let-values (((popped-operators rest-of-stack) (break
                                                                (lambda (x)
                                                                  (eqv? x lparen))
                                                                operator-stack)))
                (shunting-yard-impl (cdr tokens)
                                    (cdr rest-of-stack) ; remove lparen at top
                                    (append (reverse popped-operators) output))))
            (else (error "Invalid token" lookahead))))
          (append (reverse output) operator-stack)))

    (define-record-type <operator-attr>
      (operator-attr precedence associativity)
      operator-attr?
      (precedence operator-precedence)
      (associativity operator-associativity))

    (define operators
      (table ()
        ((+) (operator-attr 1 'left))
        ((-) (operator-attr 1 'left))
        ((*) (operator-attr 2 'left))
        ((/) (operator-attr 2 'left))
        ((expt) (operator-attr 3 'right))))

    (define (operator? token)
      (table-has-key? operators token))

    (define (precedence op)
      (operator-precedence (table-ref operators op)))

    (define (left-associative? op)
      (eqv? (associativity op) 'left))

    (define (associativity op)
      (operator-associativity (table-ref operators op)))

    (define lparen #\x28)
    (define rparen #\x29)))
