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

(defun parse-input (input-string)
  (let ((rules-hash (make-hash-table :test 'equal))
        (update-list (list)))
    (multiple-value-bind (rules updates)
        (loop for ss in (str:split #\newline input-string)
              if (find #\| ss)
                collect ss into rules
              else if (find #\, ss)
                     collect ss into updates
              finally (return (values rules updates)))
      (dolist (rule rules)
        (destructuring-bind (before after) 
            (mapcar (lambda (s)
                      (parse-integer s))
                    (str:split #\| rule))
          (push after (gethash before rules-hash (list)))))
      (dolist (update updates)
        (push (mapcar #'parse-integer (str:split #\, update)) update-list)))
    (values rules-hash update-list)))

(defun middle-page (page-update-list)
  ;; i hope all updates are of odd length
  (when (evenp (length page-update-list))
    (warn "even-length update encountered: ~a" page-update-list))
  (nth (floor (/ (length page-update-list) 2)) page-update-list))

(defun valid-update (rules update)
  (loop for (page . following) on update
        always (subsetp following (gethash page rules nil))))

(defun p1 (rules updates)
  (loop for update in updates
        if (valid-update rules update)
          sum (middle-page update)))

(defun p2 (rules updates)
  (labels ((sort-pages (p1 p2)
             (member p2 (gethash p1 rules nil))))
    (loop for update in updates
          unless (valid-update rules update)
            sum (middle-page (sort update #'sort-pages)))))

(defun test ()
  (multiple-value-bind (rules updates) (parse-input +test-input+)
    (format t "~&test part 1: ~a" (p1 rules updates))
    (format t "~&test part 2: ~a" (p2 rules updates))))

(defun main ()
  (let* ((infile-name (format nil +input-name-template+ +day-number+)))
    (multiple-value-bind (rules updates) (parse-input (uiop:read-file-string infile-name))
      (format t "~&Part 1: ~a" (p1 rules updates))
      (format t "~&Part 2: ~a" (p2 rules updates))
     )))
