(define-library (parser)
  (export operator?
          rpn-to-sexpr
          shunting-yard)
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

    (define (rpn-to-sexpr rpn)
      (rpn-to-sexpr-impl rpn '()))

    (define (rpn-to-sexpr-impl rpn stack)
      (if (not (null? rpn))
        (let ((lookahead (car rpn)))
          (cond
            ((number? lookahead) (rpn-to-sexpr-impl (cdr rpn)
                                                    (cons lookahead stack)))
            ((operator? lookahead)
              (let* ((op lookahead)
                     (rhs (car stack))
                     (lhs (cadr stack))
                     (rest-of-stack (cddr stack))
                     (sexpr (list op lhs rhs)))
                (rpn-to-sexpr-impl (cdr rpn)
                                   (cons sexpr rest-of-stack))))
            (else (error "Invalid token" lookahead))))
        (car stack)))

    (define-record-type <operator-attr>
      (operator-attr precedence associativity)
      operator-attr?
      (precedence operator-precedence)
      (associativity operator-associativity))

    (define operators
      (table ()
        (('+) (operator-attr 1 'left))
        (('-) (operator-attr 1 'left))
        (('*) (operator-attr 2 'left))
        (('/) (operator-attr 2 'left))
        (('expt) (operator-attr 3 'right))))

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
