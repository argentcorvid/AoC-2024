;;;day 5

(defparameter +day-number+ 5)
(defparameter +input-name-template+ "2024d~dinput.txt")

(defparameter +test-input+
"47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defun parse-input (lines)
  )

(defun p1 ()
  ) 

(defun p2 ()
  )

(defun test ()
  (fresh-line)
  (princ "part 1: ")
  (princ (p1 +test-input+)))

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
