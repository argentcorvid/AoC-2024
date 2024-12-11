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
  (declare (optimize (debug 3)))
  (let ((a-locs (get-x-locs data-lines #\A))
        (look-increments (list (list 1 1) (list -1 1)))
        (row-len (1- (length (first data-lines))))
        (col-len (1- (length data-lines))))
    (loop for a-posn in a-locs
          counting (= 2 (count-if (lambda (itm)
                                    (or (string= "MAS" itm) 
                                        (string= "SAM" itm)))
                                  (loop for dir in look-increments
                                        collect (loop ;for i from 0
                                                      ;this part is wrong
                                                      for j from -1 upto 1 ; index into "MAS"
                                                      for row-offset = (* j (first dir)) 
                                                      for col-offset = (* j (second dir))
                                                      for look-row = (+ (first a-posn) row-offset)
                                                      for look-col = (+ (second a-posn) col-offset)
                                                      if (and (<= 0 look-row row-len)
                                                              (<= 0 look-col col-len))
                                                        collect (elt (elt data-lines look-row) look-col) into word
                                                      else
                                                        collect #\. into word
                                                      end
                                                      finally (return (coerce word 'string)))))))))

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
