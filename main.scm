(import (scheme base)
        (scheme eval)
        (scheme write)
        (scheme process-context)
        (tokens)
        (output)
        (parser))

(define (main)
  (if (eqv? 2 (length (command-line)))
      (repl)
      (repl)))

(define (repl)
  (define (loop)
    (display "> ")
    (let ((line (read-line)))
      (if (not (or (string=? line "quit") (string=? line "exit")))
        (begin
          (call-with-current-continuation
            (lambda (k)
              (with-exception-handler
                (lambda (e)
                  (print "Error:" e)
                  (k 'e))
                (lambda ()
                  (let* ((tokens (tokenize line))
                        (sexp (infix->sexp tokens))
                        (env (environment '(scheme base))))
                    (print (eval sexp env)))))))
          (loop)))))
  (loop))

(main)
