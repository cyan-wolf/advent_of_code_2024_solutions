;; Reads the input from the given filename.
(defun read-input (filename)
  (let* ((lines (uiop:read-file-lines filename))
         (reading-rules t)
         (rules-map (make-hash-table))
         (updates-list (list)))
    (progn
      (loop for line in lines do
            (if reading-rules
                ; Parse the rules.
                (if (string= line "")
                    (setq reading-rules nil)
                    (let* ((rules-list (uiop:split-string line :separator "|")) ; Split the line.
                           ; Parse the integers.
                           (rules-nums (mapcar #'parse-integer rules-list))
                           ; Get both parsed numbers.
                           (num-before (nth 0 rules-nums))
                           (num-after (nth 1 rules-nums)))
                      ; Collect all the numbers that should go before `num-after`.
                      (pushnew num-before (gethash num-after rules-map))))
                ; Parse the updates.
                (let* ((split-line (uiop:split-string line :separator ","))
                       (nums (mapcar #'parse-integer split-line)))
                  (push nums updates-list))))
      ; Return the rules and the list of updates.
      (values rules-map updates-list)
      )))

;; Entry point for the program.
(defun main ()
  (let ((filename "input_test.txt"))
    (multiple-value-bind (rules-map updates-list) (read-input filename)
      (let ((sum 0))
        (progn
          (loop for update in updates-list do
                ; TODO
                (print update))
          
          (print sum))))
      ))