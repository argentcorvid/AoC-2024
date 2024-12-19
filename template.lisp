;;;dayx

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str))
  #+SBCL (add-package-local-nickname 'a 'alexandria)
  #+ECL  (ext:add-package-local-nickname 'a 'alexandria))

(defparameter *day-number* x)
(defparameter *input-name-template* "2024d~dinput.txt")

(defparameter *test-input*
  '())

(defun parse-input (lines)
  )

(defun p1 ()
  ) 

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
         (data (parse-input input-lines)))
    (run parts data)))

(defun test (parts)
  (run parts (parse-input *test-data*)))
