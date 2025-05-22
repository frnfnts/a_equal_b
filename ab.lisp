(defstruct command
  src
  tgt
  src-decorator
  tgt-decorator)

(defun read-lines ()
  (let ((lines nil))
    (with-open-file (input-stream "input.txt" :direction :input :element-type 'character)
      (loop for line = (read-line input-stream nil nil)
            while line
            do (push line lines)))
      (reverse lines)))

; # から始まるコマンドを削除
(defun remove-comment (commands)
  (remove-if (lambda (line) (eq (char line 0) '#\#)) commands))

; コマンドの parse
(defun split (chr str)
  (defun token-to-str (token)
    (coerce (reverse token) 'string))
  (defun f (strlist token acc)
    (cond
     ((null strlist) (reverse (push (token-to-str token) acc)))
     ((eq (car strlist) chr) (f (cdr strlist) (list) (push (token-to-str token) acc)))
     (t (f (cdr strlist) (push (car strlist) token) acc))))
  (f (coerce str 'list) (list) (list)))

(defun take-decorator (term)
  (let ((r-paren-index (search ")" term)))
    (if (and (< 0 (length term)) (equal (char term 0) #\() r-paren-index)
      (cons (subseq term 1 r-paren-index)
            (subseq term (+ r-paren-index 1)))
      (cons nil term))))
(defun parse-command (command)
  (let* ((terms (split #\= command))
         (src (take-decorator (car terms)))
         (tgt (take-decorator (cadr terms))))
    (make-command :src (cdr src) :tgt (cdr tgt) :src-decorator (car src) :tgt-decorator (car tgt))))
(defun parse-commands (lines)
  (mapcar #'parse-command (remove-comment lines)))

; コマンドを実行
(defun execute-command (command text)
  (let* ((src (command-src command))
        (tgt (command-tgt command))
        (idx (search src text)))
    (if (null idx)
      text
      (with-output-to-string (stream)
        (write-string (subseq text 0 idx) stream)
        (write-string tgt stream)
        (when (> (length text) (+ idx (length src)))
          (write-string (subseq text (+ idx (length src))) stream))))))
(defun appliable-p (command text)
  (search (command-src command) text))
(defun execute-commands (commands text)
  (let* ((appliable-commands (remove-if-not #'(lambda (cmd) (appliable-p cmd text)) commands)))
    (if (null appliable-commands)
      text
      (execute-commands commands (execute-command (car appliable-commands) text))
  )
))
