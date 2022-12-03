(import (scheme base)
        (scheme write)
        (chibi test)
        (tokens)
        (strings))

(test "string-split" (list "abc" "def" "ghi") (string-split "abc def ghi" #\space))
(test "string-split-single" (list "test") (string-split "test" #\space))

(test "concat" "a1b2" (concat "a" 1 "b" 2))
