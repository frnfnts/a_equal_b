(defparameter *text* "ABCDA")
(defparameter *commands* nil)

; input.txt から読み取る -> *commands*
(defun read-commands ()
  (let ((commands nil))
    (with-open-file (input-stream "input.txt" :direction :input :element-type 'character)
      (loop for line = (read-line input-stream nil nil)
            while line
            do (push line commands)))
      (reverse commands)))

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

(defun parse-command (command)
  (split #\= command))

; コマンドを実行
(defun execute-command (command text)
  (let ((idx (search (car command) text))
       (src (car command))
       (tgt (cadr command)))
    (if (null idx)
      text
      (with-output-to-string (stream)
        (write-string (subseq text 0 idx) stream)
        (write-string tgt stream)
        (when (> (length text) (+ idx (length src)))
          (write-string (subseq text (+ idx (length src))) stream))))))
(defun execute-commands (commands text)
  (let* ((appliable-commands (remove-if-not #'(lambda (cmd) (search(car cmd) text)) commands)))
    (if (null appliable-commands)
      text
      (execute-commands commands (execute-command (car appliable-commands) text))
  )
))

; main
(setf *commands*
  (mapcar #'parse-command (remove-comment (read-commands))))
(print *commands*)
(print (execute-commands *commands* *text*))
