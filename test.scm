(import (scheme base)
        (scheme write)
        (chibi test)
        (parser)
        (tokens)
        (strings))

(test "string-split" (list "abc" "def" "ghi") (string-split "abc def ghi" #\space))
(test "string-split-single" (list "test") (string-split "test" #\space))

(test "concat" "a1b2" (concat "a" 1 "b" 2))

; ======== parser ========
(test "operator?-plus" #t (operator? +))
(test "operator?-number" #f (operator? 123))
(test "shunting-yard-simple" (list 3 4 +)
                             (shunting-yard (list 3 + 4)))
(test "shunting-yard" (list 3 4 2 * 1 5 - 2 3 expt expt / +)
                      (shunting-yard (list 3 + 4 * 2 / #\( 1 - 5 #\) expt 2 expt 3)))
