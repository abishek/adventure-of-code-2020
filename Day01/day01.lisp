(defun remainder-to-find (total num)
  (- total num))

(defun product-of-two-expenses-adding-up-to (expenses &optional (sum 2020))
  (let* ((num (first expenses))
        (rem (remainder-to-find sum num))
        (others (rest expenses)))
    (if (find rem others)
        (* num rem)
        (if (> (length others) 0)
            (product-of-two-expenses-adding-up-to others sum)
            nil))))

(defun product-of-three-expenses-adding-up-to (expenses &optional (sum 2020))
  (let* ((num (first expenses))
         (rem (remainder-to-find sum num))
         (others (rest expenses))
         (two-expenses (product-of-two-expenses-adding-up-to others rem)))
    (if two-expenses
        (* num two-expenses)
        (product-of-three-expenses-adding-up-to others sum))))

(defun read-expenses-from-file (fname)
  (with-open-file (fp fname)
    (when fp
      (loop for line = (read-line fp nil)
            while line collect (parse-integer line)))))

(format t "~%~a~%" (product-of-two-expenses-adding-up-to (read-expenses-from-file "./input.txt")))
(format t "~%~a~%" (product-of-three-expenses-adding-up-to (read-expenses-from-file "./input.txt")))
