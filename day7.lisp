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
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (labels ((%possible (prev nl)
             (destructuring-bind (a . rest) nl
               (when (endp rest)
                 (return-from %possible (= prev a)))
               (loop for op in ops
                     when (%possible (funcall op prev a)
                                     rest)
                       do (return-from %possible t)))))
    (%possible result (nreverse number-list))))

(defun p1 (lines)
  (loop for line in lines
        for (result . numbers) = (mapcar (a:rcurry #'parse-integer :junk-allowed t)
                                         (str:split #\space line :omit-nulls t))
        when (possible result numbers)
          sum result)) 

(defun p2 ()
  )

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
