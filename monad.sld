(define-library (monad)
  (export =<<
          just
          just?
          maybe?
          nothing
          nothing?)
  (import (scheme base)
          (scheme cxr)
          (srfi 26)
          (chibi)
          (chibi generic)
          (util))
  (begin
    ; (=<< <func> <monad>)
    ; where func takes the arguments <unit> <data> and returns a monad
    (define-generic =<<)

    ; (monad <name> <return> <context> ...)
    ; where return is a function that takes arguments and returns a monad
    ; where context is (<context-name> <fields> <bind>)
    ; where fields is a list of symbols held by the monad
    ; and bind takes the arguments <func> <data> and returns a monad
    (define-syntax monad
      (sc-macro-transformer
        (lambda (exp env)
          (let* ((name-symbol (cadr exp))
                (name? (symbol-append name-symbol '?))
                (return-func (caddr exp))
                (return-name (symbol-append name-symbol '-return))
                (contexts (cdddr exp))
                (context-names (map (cut car <>) contexts))
                (context-names? (map (cut symbol-append <> '?) context-names))
                (context-tests (map (lambda (x) `(,x y)) context-names?))
                (monad-contexts (map (lambda (x) `(monad-context ,@x)) contexts)))
            `(begin
              ,@monad-contexts
              (define ,return-name ,return-func)
              (define (,name? y) (or ,@context-tests)))))))

    (define-syntax monad-context
      (sc-macro-transformer
        (lambda (exp env)
          (let* ((name (cadr exp))
                (name? (symbol-append name '?))
                (<name> (symbol-append '< (symbol-append name '>)))
                (fields (caddr exp))
                (field-names (map (lambda (x) `(,x ,(symbol-append (symbol-append name '-) x))) fields))
                (bind-func (cadddr exp)))
            `(begin
              (define-record-type ,<name> (,name ,@fields) ,name? ,@field-names)
              (define-method (=<< f (x ,name?)) (,bind-func f x)))))))

    ; ======== maybe ========
    (monad maybe
      just
      (nothing ()
            (lambda (f x)
              (nothing)))
      (just (data)
            (lambda (f x)
              (f (just-data x)))))

    (define (maybe? x)
      (or (just? x) (nothing? x)))))
