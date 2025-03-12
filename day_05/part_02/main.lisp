
(defconstant FILENAME "input_test.txt")

(defun main ()
  (let* ((lines (uiop:read-file-lines FILENAME)))
    (loop for line in lines do
          (print line))))
