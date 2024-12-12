;;;dayx

(defparameter +day-number+ x)
(defparameter +input-name-template+ "2024d~dinput.txt")

(defparameter +test-input+
  '())

(defun parse-input (lines)
  )

(defun p1 ()
  ) 

(defun p2 ()
  )

(defun run (parts-list data)
  (unless (consp parts-list)
    (setf parts-list (list parts-list)))
  (dolist (part parts-list)
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 data)))
      (2 (format t "~&Part 2: ~a" (p2 data))))))

(defun main (parts-list)
  (let* ((infile-name (format nil +input-name-template+ +day-number+))
         (input-lines (uiop:read-file-lines infile-name))
         (data (parse-input input-lines)))
    (run parts-list data)))

(defun test (parts-list)
  (run parts-list (parse-input +test-data+)))
