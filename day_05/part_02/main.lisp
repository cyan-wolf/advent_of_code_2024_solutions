
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
                      ; Collect all the numbers that should go after `num-before`.
                      (pushnew num-after (gethash num-before rules-map))))
                ; Parse the updates.
                (let* ((split-line (uiop:split-string line :separator ","))
                       (nums (mapcar #'parse-integer split-line)))
                  (push nums updates-list))))
      ; Return the rules and the list of updates.
      (values rules-map updates-list)
      )))

; Sorts the update using the given rules. Returns true if a sort was necessary.
(defun sort-update (update rules-map)
  (let ((was-sorted nil)
        (idx-last-sorted -1)
        (idx-curr 0)
        (idx-cursor nil)
        (len (length update))
        (needed-insert nil))
    
    (progn (loop while (< idx-last-sorted (- len 1)) do
                 (progn (setf idx-cursor (+ idx-curr 1))
                        (setf needed-insert nil)
                        (loop while (< idx-cursor len) do
                              (let ((curr (nth idx-curr update))
                                    (cursor (nth idx-cursor update)))

                                (if (member curr (gethash cursor rules-map))
                                    (progn () ; TODO: remove element at idx-cursor
                                           () ; TODO: insert cursor elem at idx-curr
                                           (setf needed-insert t)
                                           (setf idx-cursor len)) ; ad-hoc way of breaking out of inner loop 
                                    (incf idx-cursor))) )
                        (unless needed-insert
                          (incf idx-curr)
                          (setf was-sorted t))
                        )))

           ; Return whether the list required sorting.
           was-sorted))

; Placeholder stub.
(defun sort-update-stub (update rules-map) t)

; Gets the middle element of a sequence (such as a list).
(defun get-middle (seq)
  (let ((len (length seq)))
    (nth (floor len 2) seq)))

; Inserts a value at the specified index.
(defun list-insert-at (lst index new-value)
  (let ((retval nil))
    (loop for i from 0 to (- (length lst) 1) do
          (when (= i index)
            (push new-value retval))
          (push (nth i lst) retval))
    (when (>= index (length lst))
      (push new-value retval))
    (nreverse retval)))

; Processes the update using the given rules.
; Returns the middle element if it was incorrectly ordered, otherwise 0.
(defun process-update (update rules-map)
  (if (sort-update-stub update rules-map)
      (print (get-middle update))
      ; If the update was sorted, then the middle should not be added.
      0))

;; Entry point for the program.
(defun main ()
  (let ((filename "input_test.txt"))
    (multiple-value-bind (rules-map updates-list) (read-input filename)
      (let ((sum 0))
        (progn
          ; Get the sum of the incorrectly ordered updates.
          (loop for update in updates-list do
                (setf sum (+ sum (process-update update rules-map))))
          
          (print sum)
          )))
      ))
