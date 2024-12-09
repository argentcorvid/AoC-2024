;;;day 4

(defparameter +day-number+ 4)
(defparameter +input-name-template+ "2024d~dinput.txt")

(defparameter +test-input+
"MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX") ;; XMAS 18 times f,b,u,d, +diag

(defparameter *look-increments*
  '((1 0)  ; right
    (-1 0) ; left
    (0 1)  ; down
    (0 -1) ; up
    (1 1)  ; down-right
    (-1 1) ; down-left 
    (-1 -1); up-left
    (1 -1) ; up-right
    ))

(defparameter *xmas* "XMAS")

(defun get-x-locs (in-lines ch)
  (loop for r in in-lines
        for row from 0
        nconcing (loop for c across r
                       for col from 0
                       when (char= ch c)
                         collect (list row col))))

(defun p1 (data-lines &optional (ch #\X))
  (declare (optimize (debug 3)))
  (let ((x-locs (get-x-locs data-lines ch))
        (count 0))
    (dolist (x-posn x-locs count)
      (dolist (dir *look-increments*)
        (handler-case
            (when (loop for cand across *xmas*
                        for i from 0
                        for row-offset = (+ (first x-posn) (* i (first dir)))
                        for col-offset = (+ (second x-posn) (* i (second dir)))
                        always (char= cand (elt (elt data-lines row-offset) col-offset)))
              (incf count))
          (type-error () #\.)))))) ; return . if indexing off the end of the grid, negative or positive

(defun p2 (data-lines)
  (let ((*xmas* "MAS")
        (*look-increments* (remove-if (lambda (itm)
                                        (member 0 itm :test #'=))
                                      *look-increments*)))
    (break)
    (p1 data-lines #\M)))

(defun test ()
  (let ((lines (str:split #\newline +test-input+ :omit-nulls t)))
    (print "test p1:")
    (print (p1 lines))
    (print "test p2:")
    (print (p2 lines))))

(defun main ()
  (let* ((infile-name (format nil +input-name-template+ +day-number+))
         (input-lines (uiop:read-file-lines infile-name))
         (data input-lines))
    (fresh-line)
    (princ "part 1: ")
    (princ (p1 data))
    (fresh-line)
    (princ "part 2: ")
    (princ (p2 data))
    ))
