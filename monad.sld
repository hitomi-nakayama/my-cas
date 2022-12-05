(define-library (monad)
  (export =<<
          just
          just?
          maybe?
          nothing
          nothing?)
  (import (scheme base)
          (chibi generic))
  (begin
    ; (=<< <func> <monad>)
    ; where func takes the arguments <unit> <data> and returns a monad
    (define-generic =<<)

    ; ======== maybe ========
    (define-record-type <nothing>
      (nothing)
      nothing?)
    (define-method
      (=<< f (a nothing?))
        (nothing))

    (define-record-type <just>
      (just a)
      just?
      (a just-data))
    (define-method
      (=<< f (a just?))
        (f just (just-data a)))

    (define (maybe? x)
      (or (just? x) (nothing? x)))))
