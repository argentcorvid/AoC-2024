;;;day 8

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str :cl-ppcre))
  #+SBCL (add-package-local-nickname 'a 'alexandria)
  #+ECL  (ext:add-package-local-nickname 'a 'alexandria))

(defparameter *day-number* 8)
(defparameter *input-name-template* "2024d~dinput.txt")

(defparameter *test-input*
  "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
")

(defun parse-input (lines)
  (let ((grid (make-hash-table))
        (antenna-scanner (ppcre:create-scanner "[0-9a-zA-Z]")))
    (loop for l from 0
          for line in lines
          do (ppcre:do-matches (c end antenna-scanner line)
               (let ((antenna-value (char line c)))
                 (setf (gethash (complex l c) grid) antenna-value))))
    grid))

(defun p1 ()
  ) 

(defun p2 ()
  )

(defun run (Zparts-list data)
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
