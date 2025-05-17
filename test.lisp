(load "ab.lisp")
(defun test (name actual expected)
  (unless (equal actual expected)
    (format t "test failed: ~a: ~a ~a~%" name actual expected)))

(test "remove-comment"
  (remove-comment '("A=B" "#B=C" "#C=D" "D=E" "#E=F" "#F=G" "#G=A"))
  '("A=B" "D=E"))

;; Test for split function
(test "split"
  (split #\= "A=B")
  '("A" "B"))

(test "split-multiple"
  (split #\= "hello=world=test")
  '("hello" "world" "test"))

;; Test for parse-command function
(test "parse-command"
  (parse-command "hello=world")
  '("hello" "world"))

(test "parse-command-empty"
  (parse-command "hello=")
  '("hello" ""))

;; Test for execute-command function
(test "execute-command"
  (execute-command '("hello" "hi") "hello world")
  "hi world")

(test "execute-command-multiple"
  (execute-command '("l" "L") "hello world")
  "heLlo world")

(test "execute-command-not-found"
  (execute-command '("xyz" "abc") "hello world")
  "hello world")

;; Test for execute-commands function
(test "execute-commands"
  (execute-commands '(("hello" "hi") ("world" "earth")) "hello world")
  "hi earth")

(test "execute-commands-recursive"
  (execute-commands '(("hello" "hi") ("hi" "hey")) "hello world")
  "hey world")

(test "execute-commands-no-match"
  (execute-commands '(("xyz" "abc") ("123" "456")) "hello world")
  "hello world")

;; Test for parse-commands function
(test "parse-commands-basic"
  (parse-commands '("A=B" "C=D"))
  '(("A" "B") ("C" "D")))

(test "parse-commands-with-comments"
  (parse-commands '("A=B" "#X=Y" "C=D"))
  '(("A" "B") ("C" "D")))

(test "parse-commands-empty"
  (parse-commands '())
  '())

(test "parse-commands-only-comments"
  (parse-commands '("#A=B" "#C=D"))
  '())

(test "parse-commands-with-empty-values"
  (parse-commands '("A=" "B=C"))
  '(("A" "") ("B" "C")))

;; Complete test workflow
(test "full-workflow"
  (let ((filtered-commands (remove-comment (list "hello=world" "#skip=this"))))
    (execute-commands (parse-commands filtered-commands) "hello test"))
  "world test")
