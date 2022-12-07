(import (scheme base)
        (scheme write)
        (srfi 1)
        (srfi 26)
        (chibi test)
        (constants)
        (monad)
        (parser)
        (table)
        (tokens)
        (strings))

(test-begin "tests")

; ======== monad ========
; (let ((add-one (lambda (unit x) (unit (+ x 1)))))
;   (test "just-add-one-just" (just 2) (=<< add-one (just 1)))
;   (test "just-add-one-nothing" (nothing) (=<< add-one (nothing)))
;   (test "just-add-one-associativity" (=<< (lambda (u x) (=<< add-one (add-one u x))) (just 1))
;                                      (=<< add-one (=<< add-one (just 1)))))

; ======== parser ========
(test "operator?-plus" #t (operator? '+))
(test "operator?-number" #f (operator? 123))
(test "shunting-yard-simple" '(3 4 +)
                             (shunting-yard '(3 + 4)))
(test "shunting-yard" '(3 4 2 * 1 5 - 2 3 expt expt / +)
                      (shunting-yard '(3 + 4 * 2 / #\( 1 - 5 #\) expt 2 expt 3)))
(test "rpn-to-sexp-add-mul" '(+ 3 (* 4 2))
                            (rpn-to-sexp '(3 4 2 * +)))
(test "rpn-to-sexp-expt" '(expt  2 8)
                            (rpn-to-sexp '(2 8 expt)))

; ======== strings ========
(test "concat" "a1b2" (concat "a" 1 "b" 2))

; ======== table ========
(let ((my-table (table (string=?) (("a") 1) (("b") 2))))
  (test-assert "table-keys" (list= string=? '("a" "b") (table-keys my-table)))
  (test "table-find-pair" (cons "a" 1) (table-find-pair my-table "a"))
  (test "table-ref-exists" 1 (table-ref my-table "a"))
  (test-error "table-ref-not-exist" (table-ref my-table "z"))
  (test "table-has-key?-true" #t (table-has-key? my-table "a"))
  (test "table-has-key?-false" #f (table-has-key? my-table "w")))

(let ((my-table (table () ((1) 11) ((2) 22))))
  (test-assert "table-no-key-eq-keys" (list= eqv? '(1 2) (table-keys my-table)))
  (test "table-find-no-key-eq-pair" (cons 1 11) (table-find-pair my-table 1)))

; ======== tokens ========
(test "tokenize" '(1 2 3) (tokenize "1 2 3"))
(test "tokenize-expression" `(2 * (unquote lparen) 1 + 3 (unquote rparen))
                            (tokenize "2 * (1 + 3)"))
(test "string-split" (list "abc" "def" "ghi") (string-split "abc def ghi" (cut eqv? #\space <>)))
(test "string-split" (list "(" "a") (string-split "(a" (cut eqv? lparen <>)))
(test "string-split-single" (list "test") (string-split "test" (cut eqv? #\space <>)))


(test-end "tests")
(test-exit)
