;;;day 8

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str :cl-ppcre))
  #+SBCL (add-package-local-nickname 'a 'alexandria)
  #+ECL  (ext:add-package-local-nickname 'a 'alexandria))

(defparameter *day-number* 8)
(defparameter *input-name-template* "2024d~dinput.txt")

(defparameter *test-input*
 (str:split #\newline "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
"
            :omit-nulls t))

(defun parse-input (lines)
  (let ((grid (make-hash-table))
        (antenna-scanner (ppcre:create-scanner "[0-9a-zA-Z]")))
    (loop for l from 0
          for line in lines
          do (ppcre:do-matches (c end antenna-scanner line)
               (let ((antenna-value (char line c)))
                 (push (complex l c) (gethash antenna-value grid)))))
    (setf (gethash 'rows grid) (length lines))
    (setf (gethash 'cols grid) (length (first lines)))
    grid))

(defun p1 (grid)
  (let ((antinodes (make-hash-table))
        (maxrow (1+ (gethash 'rows grid)))
        (maxcol (1+ (gethash 'cols grid))))
    (maphash (lambda (freq locs)
               (when (and (characterp freq) ;becuase of rows and cols
                          (< 1 (length locs)))
                 (a:map-combinations (lambda (points)
                                       (destructuring-bind (pt1 pt2) points
                                         (let* ((dist (- pt2 pt1))
                                                (an1 (+ pt1 (* 2 dist)))
                                                (an2 (+ pt2 (* -2 dist))))
                                           (dolist (an (list an1 an2))
                                             (when (and (< 0 (realpart an) maxrow)
                                                        (< 0 (imagpart an) maxcol))
                                               (setf (gethash an antinodes) t))))))
                                     locs :length 2)))
             grid)
    antinodes)) 

(defun p2 ()
  )

(defun run (parts-list data)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (let ((p1-answer (hash-table-count (p1 data))))
           (format t "~&Part 1: ~a" p1-answer)))
      (2 (format t "~&Part 2: ~a" (p2 data))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input-lines (uiop:read-file-lines infile-name))
         (data (parse-input input-lines)))
    (run parts data)))

(defun test (parts)
  (run parts (parse-input *test-input*)))
