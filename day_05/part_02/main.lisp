
;; Reads the input from the given filename.
(defun read-input (filename)
  (let* ((lines (uiop:read-file-lines filename))
         (reading-rules t)
         (rules-list ())
         (updates-list ()))
    (progn
      (loop for line in lines do
            (if reading-rules
                ; Parse the rules.
                (if (string= line "")
                    (setq reading-rules nil)
                    (push line rules-list))
                ; Parse the updates.
                (let* ((split-line (uiop:split-string line :separator ","))
                       (nums (mapcar #'parse-integer split-line)))
                  (push nums updates-list))))
      ; Return the rules and the list of updates.
      (values rules-list updates-list)
      )))

;; Entry point for the program.
(defun main ()
  (let ((filename "input_test.txt"))
    (multiple-value-bind (rules-list updates-list) (read-input filename)
      ; TODO
      (progn
        (print rules-list)
        (print updates-list))

      )))