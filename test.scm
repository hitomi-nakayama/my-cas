(import (scheme base)
        (scheme write)
        (chibi test)
        (tokens))

(test "string-split" (list "abc" "def" "ghi") (string-split "abc def ghi" #\space))
(test "string-split-single" (list "test") (string-split "test" #\space))
