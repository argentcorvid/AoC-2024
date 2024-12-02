;;;dayx
(eval-when (:compile-toplevel :load-toplevel :execute) (ql:quickload (:str :alexandria)))

(eval-when (:compile-toplevel)

  (import '())
  
  (defconstant +day-number+ x)
  (defconstant +working-dir+ (uiop:truenamize "~/aoc_2023/"))
  (defconstant +input-name-template+ "2023d~dinput.txt")

  (defconstant +test-input+
    '()))

(defun parse-input (lines)
  )

(defun p1 ()
  ) 

(defun p2 ()
  )

(defun main ()
  (let* ((infile-name (format nil +input-name-template+ +day-number+))
         (input-lines (uiop:read-file-lines infile-name))
         (data (parse-input input-lines)))
    (fresh-line)
    (princ "part 1: ")
    (princ (p1 data))
    (fresh-line)
    (princ "part 2: ")
    (princ (p2 data))))
