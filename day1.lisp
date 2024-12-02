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

(defun parse-input-line (line)
  (let ((str:*omit-nulls* t))
    (mapcar #'parse-integer (str:split #\space line))))

(defun parse-input (lines)
  (mapcar #'parse-input-line lines))

(defun transpose-2d (lst)
  (apply #'mapcar #'list lst))

(defun p1 (data-in)
  (apply #'+ (mapcar (lambda (l)
                       (abs (apply #'- l)))
                     (transpose-2d (mapcar (lambda (i)
                                      (sort i #'<))
                                    (transpose-2d data-in))))))

(defun p2 (data-in)
  (let* ((td (transpose-2d data-in))
         (left (first td))
         (right (second td)))
    (reduce #'+ (mapcar (lambda (itm)
                          (* itm (count itm right)))
                        left))))

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
