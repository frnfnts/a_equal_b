(defparameter *text* "ABCDA")
(defparameter *commands* nil)
(load "ab.lisp")
; main
(setf *commands* (parse-commands (read-lines)))
(print (execute-commands *commands* *text*))
