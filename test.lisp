(load "ab.lisp")
(defun test (name actual expected)
  (format t "~%Testing: ~a~%" name)
  (if (equal actual expected)
      (format t "  ✓ PASS~%")
      (format t "  ✗ FAIL: Expected ~S but got ~S~%" expected actual)))

(defun run-tests ()
  (format t "~%=== Running Tests ===~%")

  ;; Test for remove-comment
  (test "remove-comment - standard case"
    (remove-comment '("A=B" "#B=C" "#C=D" "D=E" "#E=F" "#F=G" "#G=A"))
    '("A=B" "D=E"))

  (test "remove-comment - empty list"
    (remove-comment '())
    '())

  (test "remove-comment - only comments"
    (remove-comment '("#A=B" "#C=D"))
    '())

  ;; Test for split function
  (test "split - simple string"
    (split #\= "A=B")
    '("A" "B"))

  (test "split - multiple delimiters"
    (split #\= "hello=world=test")
    '("hello" "world" "test"))

  (test "split - empty right side"
    (split #\= "A=")
    '("A" ""))

  (test "split - empty left side"
    (split #\= "=B")
    '("" "B"))

  ;; Test for take-decorator function
  (test "take-decorator - no parentheses"
    (take-decorator "hello")
    '(nil . "hello"))

  (test "take-decorator - with parentheses"
    (take-decorator "(abc)hello")
    '("abc" . "hello"))

  ;; Test for parse-command function
  (test "parse-command - basic command"
    (let ((cmd (parse-command "hello=world")))
      (list (command-src cmd) (command-tgt cmd)
            (command-src-decorator cmd) (command-tgt-decorator cmd)))
    '("hello" "world" nil nil))

  (test "parse-command - empty target"
    (let ((cmd (parse-command "hello=")))
      (list (command-src cmd) (command-tgt cmd)
            (command-src-decorator cmd) (command-tgt-decorator cmd)))
    '("hello" "" nil nil))

  (test "parse-command - with accessories"
    (let ((cmd (parse-command "(replace)hello=(with)world")))
      (list (command-src cmd) (command-tgt cmd)
            (command-src-decorator cmd) (command-tgt-decorator cmd)))
    '("hello" "world" "replace" "with"))

  ;; Test for parse-commands function
  (test "parse-commands - basic list"
    (let ((cmds (parse-commands '("A=B" "C=D"))))
      (list (command-src (first cmds)) (command-tgt (first cmds))
            (command-src (second cmds)) (command-tgt (second cmds))))
    '("A" "B" "C" "D"))

  (test "parse-commands - with comments"
    (let ((cmds (parse-commands '("A=B" "#X=Y" "C=D"))))
      (list (command-src (first cmds)) (command-tgt (first cmds))
            (command-src (second cmds)) (command-tgt (second cmds))))
    '("A" "B" "C" "D"))

  ;; Test for execute-command function
  (test "execute-command - simple replacement"
    (execute-command (parse-command '"hello=hi") "hello world")
    "hi world")

  (test "execute-command - multiple occurrences"
    (execute-command (parse-command '"l=L") "hello world")
    "heLlo world")

  (test "execute-command - pattern not found"
    (execute-command (parse-command '"xyz=abc") "hello world")
    "hello world")

  ;; Test for execute-commands function
  (test "execute-commands - multiple replacements"
    (execute-commands (parse-commands '("hello=hi" "world=earth")) "hello world")
    "hi earth")

  (test "execute-commands - recursive replacement"
    (execute-commands (parse-commands '("hello=hi" "hi=hey")) "hello world")
    "hey world")

  (test "execute-commands - no matches"
    (execute-commands (parse-commands '("foo=bar" "baz=qux")) "hello world")
    "hello world")

  ;; Complete workflow tests
  (test "full-workflow - simple case"
    (let* ((lines '("hello=world" "#skip=this"))
           (filtered (remove-comment lines))
           (cmds (parse-commands filtered)))
      (execute-commands cmds "hello test"))
    "world test")

  (test "full-workflow - with accessories"
    (let* ((lines '("(abc)hello=world" "#skip=this" "(xyz)test=example"))
           (filtered (remove-comment lines))
           (cmds (mapcar #'parse-command filtered)))
      (list (command-src-decorator (first cmds))
            (command-tgt (first cmds))
            (command-src-decorator (second cmds))
            (command-tgt (second cmds))))
    '("abc" "world" "xyz" "example"))

  (format t "~%=== Tests Completed ===~%"))

;; Run all tests
(run-tests)
