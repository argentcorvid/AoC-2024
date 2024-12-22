;;;day7

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str))
  #+SBCL (add-package-local-nickname 'a 'alexandria)
  #+ECL  (ext:add-package-local-nickname 'a 'alexandria))

(defparameter *day-number* 7)
(defparameter *input-name-template* "2024d~dinput.txt")

(defparameter *test-input*
  (str:split #\newline
             "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
"
             :omit-nulls t))

;(defun parse-input (lines))

(defun possible (result number-list &optional (ops (list #'/ #'-)))
  (labels ((%possible (prev nl)
             (destructuring-bind (a . rest) nl
               (when (endp rest)
                 (return-from %possible (= prev a)))
               (loop for op in ops
                     when (eql op #'/)
                       when(zerop (mod prev a))
                         when (%possible (funcall op prev a)
                                         rest)
                           return t
                     when (eql op #'decat)
                       when (endswith prev a)
                         when (%possible (funcall op prev a)
                                         rest)
                           return t
                     when (eql op #'-)
                       when (%possible (funcall op prev a)
                                         rest)
                         return t))))
    (%possible result (nreverse number-list))))

(defun endswith (num1 num2)
  (if (zerop num2)
      (zerop (mod num1 10))
      (zerop (expt (mod (- num1 num2) 10)
                   (digits num2)))))
(defun digits (n)
  (1+ (floor(log n 10))))

(defun decat (num1 num2)
  (floor num1 (expt 10 (digits num2))))

(defun p1 (lines)
  (loop for line in lines
        for (result . numbers) = (mapcar (a:rcurry #'parse-integer :junk-allowed t)
                                         (str:split #\space line :omit-nulls t))
        when (possible result numbers)
          sum result)) 

(defun p2 (lines)
  (loop for line in lines
        for (result . numbers) = (mapcar (a:rcurry #'parse-integer :junk-allowed t)
                                         (str:split #\space line :omit-nulls t))
        when (possible result numbers (list #'- #'/ #'decat))
          sum result))

(defun run (parts-list data)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 data)))
      (2 (format t "~&Part 2: ~a" (p2 data))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input-lines (uiop:read-file-lines infile-name))
        ; (data (parse-input input-lines))
         )
    (run parts input-lines)))

(defun test (&rest parts)
  (run parts *test-input*))
put*))
l *input-name-template* *day-number*))
         (input-lines (uiop:read-file-lines infile-name))
        ; (data (parse-input input-lines))
         )
    (run parts input-lines)))

(defun test (&rest parts)
  (run parts *test-input*))
put*))
