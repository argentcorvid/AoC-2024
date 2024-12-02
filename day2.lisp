;;;day2

(defparameter +day-number+ 2)
(defparameter +working-dir+ (uiop:truenamize "~/aoc-2024/"))
(defparameter +input-name-template+ "2024d~dinput.txt")

(defparameter +test-input+
  (str:split #\newline
"7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"))

(defun parse-input (lines)
  (let ((str:*omit-nulls* t))
    (mapcar (lambda (l)
              (mapcar #'parse-integer (str:split #\space l)))
            lines)))

(defun report-safe-p (report)
  (and
   (or (apply #'> report)
       (apply #'< report))
   (loop for (a b) on report while b
         for diff = (abs (- a b))
         always (and (<= 1 diff)
                     (>= 3 diff)))))

(defun p1 (report-list)
  (count-if #'report-safe-p report-list)) 

(defun p2 ()
  )

(defun main ()
  (let* ((infile-name (format nil +input-name-template+ +day-number+))
         (input-lines (uiop:read-file-lines infile-name))
         (data (parse-input input-lines)))
    (fresh-line)
    (princ "part 1: ")
    (princ (p1 data))
    ;; (fresh-line)
    ;; (princ "part 2: ")
    ;; (princ (p2 data))
    ))
