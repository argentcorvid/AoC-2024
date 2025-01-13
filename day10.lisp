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
  (let ((next-points (list)))
    (declare (special map-grid seen))
    (dolist (look-dir *directions* (nreverse next-points))
      (let* ((cand-point (mapcar #'+ current-point look-dir))
             (candidate (handler-case (apply #'aref map-grid cand-point)
                          (error () *out-of-bounds-value*)))
             (current-height (apply #'aref map-grid current-point)))
        (when (and (/= candidate *out-of-bounds-value*)
                   (= allowed-difference (- candidate current-height))
                   (not (member cand-point seen :test #'equal)))
          (push cand-point next-points))))))

(defun sum-of-trail-scores (trailscores)
  (reduce #'+ (a:hash-table-values trailscores)))

;;first try at iterative dfs - see end of file for more-generalized solution, need to spin to utility library
(defun p1 (map-grid trailheads &optional (p2 nil))
  (declare (special map-grid))
  (let ((trails-from-head (make-hash-table :test 'equal)))
    (dolist (head trailheads
                  (sum-of-trail-scores trails-from-head))
      
      (do* ((seen (list) (if p2
                             '()
                             (cons current-point seen)))
            (stack (list head) (nconc (next-trail-points current-point) stack))
            (current-point head (pop stack))
            (head-score 0 (if (= (the fixnum 9) (apply #'aref map-grid  current-point))
                              (1+ (the fixnum head-score))
                              head-score)))
           ((endp stack)
            (setf (gethash head trails-from-head) head-score))
        (declare (special seen))))))

(defun p2 (map-grid trailheads)
  (p1 map-grid trailheads t))

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


(defun dfs-iter (neighbor-fn graph initial-point &key (all-possible nil))
  "general iterative Depth-First Search on a graph.
neighbor-fn takes 2 arguments, the graph and the starting point, needs to return a list of 'valid' neighbors of that point (for whatever that means for the given graph). all-possible disables checking of visited points, so may cause infinite looping. returns the list of travelled points. "
  (check-type neighbor-fn function)
  (do* ((visited-pts (list) (cons current-pt visited-pts))
        (not-visited (list) (delete-if (lambda (itm)
                                         (if (not all-possible)
                                             (member itm visited-pts :test #'equal)
                                             nil))
                                       (funcall neighbor-fn graph current-pt)))
        (search-stack (list initial-point) (nconc not-visited search-stack))
        (current-pt initial-point (pop search-stack)))
       ((endp search-stack)             ; full traversal
        (nreverse visited-pts))))

(defun d10-next-pts (grid current-point)
  (loop for look-dir in *directions*
        for cand-point = (mapcar #'+ current-point look-dir)
        for cand-height = (handler-case (apply #'aref grid cand-point)
                            (error () *out-of-bounds-value*))
        for current-height = (apply #'aref grid current-point)
        when (= 9 current-height)
          return nil
        when (and (/= cand-height *out-of-bounds-value*)
                  (= 1 (- cand-height current-height)))
          collect cand-point))

(defun general ()
  (multiple-value-bind (g starts)
      (parse-input (uiop:read-file-lines "2024d10input.txt"))
    (loop for s in starts
          sum (count-if (lambda (pt)
                          (= 9 (apply #'aref g pt)))
                        (dfs-iter #'d10-next-pts g s))
            into pt1
          sum (count 9
                     (dfs-iter #'d10-next-pts g s :all-possible t)
                     :key (a:curry #'apply #'aref g))
            into pt2
          finally (return (values pt1 pt2)))))
