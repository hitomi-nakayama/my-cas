(define-library (tokens)
  (export tokenize)
  (import (scheme base)
          (scheme char)
          (srfi 1)
          (table))
  (begin
    ; convert an input string into a list of terminals
    (define (tokenize input)
      (let ((input-list (concatenate `(,(string->list input) (#\null)))))
        (tokenize-impl input-list 'start '() '())))

    (define (tokenize-impl input state stack output)
      (if (null? input)
        (reverse output)
        (let ((lookahead (car input)))
          (let-values (((state action) (token-machine state lookahead)))
            (case action
              ((push) ; push the character onto the stack
                (tokenize-impl (cdr input) state (cons lookahead stack) output))
              ((integer real complex) ; convert the stack into a number
                (tokenize-impl input state '() (cons (string->number (list->string (reverse stack))) output)))
              ((symbol) ; convert the stack into a symbol
                (let ((symbol (table-ref char-to-symbol (car stack))))
                  (tokenize-impl input state '() (cons symbol output))))
              ((skip)
                (tokenize-impl (cdr input) state stack output))
              (else
                (error "cannot parse character" lookahead state stack)))))))

    (define (token-machine state input)
      ; a mealy machine that takes a state and an input character
      ; and returns a new state and an action
      (case state
        ((start)
          (cond
            ((eqv? #\null input) (values 'start 'skip))
            ((eqv? #\. input) (values 'integer 'push))
            ((char-numeric? input) (values 'integer 'push))
            ((table-has-key? char-to-symbol input) (values 'symbol 'push))
            ((char-whitespace? input) (values 'start 'skip))
            (else (values 'error 'error))))
        ((integer)
          (cond
            ((char-numeric? input) (values 'integer 'push))
            ((eqv? #\. input) (values 'real 'push))
            ((eqv? #\i input) (values 'complex 'push))
            (else (values 'start 'integer))))
        ((real)
          (cond
            ((char-numeric? input) (values 'real 'push))
            ((eqv? #\i input) (values 'complex 'push))
            (else (values 'start 'real))))
        ((complex)
          (values 'start 'complex))
        ((symbol)
          (values 'start 'symbol))
        (else (values 'error 'error))))

    (define char-to-symbol (table ()
        ((#\+) '+)
        ((#\-) '-)
        ((#\*) '*)
        ((#\/) '/)
        ((#\() 'lpar)
        ((#\)) 'rpar)
        ((#\^) 'expt)))))
