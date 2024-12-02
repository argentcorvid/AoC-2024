;;;day 1

(defparameter +day-number+ 1)
(defparameter +working-dir+ (uiop:truenamize "~/aoc-2024/"))
(defparameter +input-name-template+ "2024d~dinput.txt")

(defparameter +test-input+
  (str:split #\newline
             "3   4
4   3
2   5
1   3
3   9
3   3"))

(defun parse-input (lines)
  (let ((str:*omit-nulls* t))
    (loop for l in lines
          for (lnum rnum) in (mapcar #'parse-integer (str:split #\space l))
          collect lnum into left
          collect rnum into right
          finally (return (list (sort left  #'<)
                                (sort right #'<))))))

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
