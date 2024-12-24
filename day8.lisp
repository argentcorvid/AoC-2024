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
        (maxrow (gethash 'rows grid))
        (maxcol (gethash 'cols grid)))
    (labels ((location-mapper (points)
               (destructuring-bind (pt1 pt2) points
                 (let* ((dist (- pt1 pt2))
                        (an1 (+ pt2 (* 2 dist)))
                        (an2 (+ pt2 (- dist))))
                   (dolist (an (list an1 an2))
                     (when (and (<= 0 (realpart an) (1- maxrow))
                                (<= 0 (imagpart an) (1- maxcol)))
                       (setf (gethash an antinodes) t))))))
             (grid-mapper (freq locs)
               (when (and (characterp freq) ;becuase of rows and cols
                          (< 1 (length locs)))
                 (a:map-combinations #'location-mapper
                                     locs :length 2))))
      (maphash #'grid-mapper grid))
    antinodes)) 

(defun p2 (grid)
  (let ((antinodes (make-hash-table))
        (maxrow (1- (gethash 'rows grid)))
        (maxcol (1- (gethash 'cols grid))))
    (labels ((grid-mapper(freq locs)
               (when (and (characterp freq) ;becuase of rows and cols
                          (< 1 (length locs)))
                 (a:map-combinations #'location-mapper locs :length 2)))
             (in-range (point)
               (and (<= 0 (realpart point) maxrow)
                    (<= 0 (imagpart point) maxcol)))
             (location-mapper (points)
               (destructuring-bind (pt1 pt2) points
                 (let* ((dist (- pt1 pt2)))
                   (loop for an = pt1 then (+ an dist)
                         while (in-range an)
                         do (setf (gethash an antinodes) t)
                         finally (setf (gethash pt1 antinodes) t))
                   (loop for an = pt2 then (+ an (- dist))
                         while (in-range an)
                         do (setf (gethash an antinodes) t)
                         finally (setf (gethash pt2 antinodes) t))))))
      (maphash #'grid-mapper grid))
    antinodes))

(defun run (parts-list data)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (let ((p1-answer (p1 data)))
           (format t "~&Part 1: ~a" (hash-table-count p1-answer))))
      (2 (let ((p2-answer (p2 data)))
           (format t "~&Part 2: ~a" (hash-table-count p2-answer))
          ; (break)
           )))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input-lines (uiop:read-file-lines infile-name))
         (data (parse-input input-lines)))
    (run parts data)))

(defun test (&rest parts)
  (run parts (parse-input *test-input*)))
