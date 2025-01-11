;;;day10

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str)))

(eval-when (:compile-toplevel :load-toplevel)
  (add-package-local-nickname 'a 'alexandria))

(defparameter *day-number* 10)
(defparameter *input-name-template* "2024d~dinput.txt")

(defparameter *small-test-input*
  "0123
1234
8765
9876
"
  "p1 trailhead count: 1
  score: 1")

(defparameter *med-input-2*
  "..90..9
...1.98
...2..7
6543456
765.987
876....
987....
"
  "p1 trailhead count: 1
  score: 4")

(defparameter *med-input-3*
  "10..9..
2...8..
3...7..
4567654
...8..3
...9..2
.....01
"
  "p1 trailhead count: 2
  score: 3")

(defparameter *big-test-input*
  "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
"
  "p1 trailhead count: 9
  score: 36")

(defvar *directions*
  '((-1 0) (0 1) (1 0) (0 -1)))

(defparameter *out-of-bounds-value* -1)

(defparameter *trail-start* 0)
(defparameter *trail-end*   9)

(defun parse-input (lines)
  (typecase lines (string (setf lines (str:split-omit-nulls #\newline lines)))) ;keep forgetting to split test inputs
  (loop with map-rows = (length lines)
        with map-cols  = (length (first lines))
        with grid = (make-array (list map-rows map-cols) :element-type 'fixnum :initial-element *out-of-bounds-value*)
        for row in lines
        and row-number from 0
        nconcing (loop for spot across row
                       for col-number from 0
                       for ch = (digit-char-p spot)
                       unless (null ch)
                         do (setf (aref grid row-number col-number) ch)
                         and when (= *trail-start* ch)
                               collect (list row-number col-number))
          into heads
        finally (return (values grid heads))))

(defun next-trail-points (current-point &optional (allowed-difference 1))
  (let ((next-points (list))
       ;; (c-point (apply #'complex current-point))
        )
    (declare (special map-grid seen))
    (dolist (look-dir *directions* (nreverse next-points))
      (let* ((cand-point (mapcar #'+ current-point look-dir)) ;; do i want to use complexes or lists?
             (candidate (handler-case (apply #'aref map-grid cand-point)
                          (error () *out-of-bounds-value*)))
             (current-height (apply #'aref map-grid current-point)))
        (when (and (/= candidate *out-of-bounds-value*)
                   (= allowed-difference (- candidate current-height))
                   (not (member cand-point seen :test #'equal)))
          (push cand-point next-points))))))

(defun sum-of-trail-scores (trailscores)
  (reduce #'+ (a:hash-table-values trailscores)))

(defun p1 (map-grid trailheads)
  (declare (special map-grid))
  (labels ((walk-trail (point)
             "return number of times you can reach height 9, starting from the given point"
             (declare (special seen))
             (push point seen)
             (if (= *trail-end* (apply #'aref map-grid point)) ;need to add already found check
                 (return-from walk-trail 1))
             (let ((path-cands (next-trail-points point))
                   (this-level-accum 0))  
               (loop (when (endp path-cands)
                       (return-from walk-trail this-level-accum))
                     (incf this-level-accum (walk-trail (pop path-cands)))))))
    (let ((trails-from-head (make-hash-table :test 'equal)))
      (dolist (head trailheads
                    (sum-of-trail-scores trails-from-head))
        (let ((seen (list)))
          (declare (special seen))
          (let ((head-score (walk-trail head)))
            (when (plusp head-score)
              (setf (gethash head trails-from-head) head-score))))))))

(defun p2 (map-grid trailheads)
  (declare (special map-grid))
  )

(defun run (parts-list grid trailheads)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 grid trailheads)))
      (2 (format t "~&Part 2: ~a" (p2 grid trailheads))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input-lines (uiop:read-file-lines infile-name)))
    (multiple-value-call #'run parts (parse-input input-lines))))

(defun test (&rest parts)
  (multiple-value-call #'run parts (parse-input *big-test-input*)))
