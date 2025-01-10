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
  ".90..9
...1.98
...2..7
6543456
765.987
1876....
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
  #(#c(-1 0) #c(0 1) #c(1 0) #c(0 -1)))

(defun parse-input (lines)
  (loop with map-rows = (length lines)
        with map-cols  = (length (first lines))
        with grid = (make-array (list map-rows map-cols) :element-type 'fixnum :initial-element -1)
        for row in lines
        and row-number from 0
        nconcing (loop for spot across row
                       for col-number from 0
                       for ch = (digit-char-p spot)
                       unless (null ch)
                         do (setf (aref grid row-number col-number) ch)
                         and when (zerop ch)
                         collect (list row-number col-number))
        into heads
        finally (return (values grid heads))))

(defun p1 ()
  ) 

(defun p2 ()
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
  (multiple-value-call #'run parts (parse-input *test-input*)))
