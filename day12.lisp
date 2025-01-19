;;;day12

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str))
  (add-package-local-nickname 'a 'alexandria-2))

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (use-package 'defclass-std))

(defparameter *day-number* 12)
(defparameter *input-name-template* "2024d~dinput.txt")

(defparameter *small-test-input*
  "AAAA
BBCD
BBCC
EEEC"
  "part 1: 5 regions, total price 140")

(defparameter *med-test-input*
  "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO"
  "part 1: 5 regions, total price 772")

(defparameter *test-input*
  "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"
  "part 1: 11 regions, total price 1930")

(defun parse-input (lines)
  (when (typep lines 'string)
    (setf lines (str:split-omit-nulls #\newline lines)))
  (make-array (list (length lines) (length (first lines)))
              :element-type 'character
              :initial-contents lines))

(defparameter *directions* '((0 1) (1 0) (0 -1) (-1 0)))

(defun oob? (grid itm)
  (let ((maxs (array-dimensions grid)))
    (or (some #'minusp itm)
        (some #'>= itm maxs))))

(defun same-neighbors (grid row col)
  (flet ((keyfun (point)
           (handler-case (apply #'aref grid point)
             (error () #\$))))
    (let* ((point (list row col))
           (candidates (remove-if (a:curry #'oob? grid) (mapcar (lambda (d)
                                                                  (mapcar #'+ point d))
                                                                *directions*))))
      (remove-if (a:curry #'char-not-equal (aref grid row col))
                 candidates
                 :key #'keyfun))))

(defun find-region-from-point (grid row col)
  " floodfill to fimd adjacent plots"
  (do* ((visited (list) (cons current-point visited))
        (current-point (list row col) (pop search-queue))
        (neighbors (same-neighbors grid row col)
                   (apply #'same-neighbors grid current-point))
        (perimeter (- 4 (length neighbors))
                   (- 4 (length neighbors)))
        (not-visited neighbors
                     (set-difference neighbors visited :test (lambda (n v) (equal n (car v)))))
        (search-queue not-visited (union not-visited search-queue :test #'equal)))
       ((endp search-queue)
        (pushnew (cons current-point perimeter) visited :key #'car :test #'equal)
        (nreverse visited))
    (setf current-point (cons current-point perimeter))
    ;;(break)
    ))

(defun p1 (garden)
  (let* ((table-size (+ 10 (* 2 26)))   ; A-Z,a-z,0-9
         (garden-size (array-total-size garden))
         (garden-dims (array-dimensions garden))
         
         (plot-perims (make-hash-table :size table-size))
         (linear-garden (make-array garden-size :displaced-to garden :element-type 'character)))
    (loop for plot across linear-garden
          for index from 0
          for col = (mod index (second garden-dims))
          for row = (floor (- index col) (first garden-dims))
          for sneighbors = (length (same-neighbors garden row col)) 
          #|  do (incf (gethash plot plot-perimeters 0) (- 4 sneighbors))  ;no, need to multiply perm and area, then add
                           (incf (gethash plot plot-areas 0))|#
          do ?) 
    (break) 
    (loop for p being the hash-values of plot-perims using (hash-key type )
          summing (* p (count type linear-garden)))))
 

(defun p2 (garden)
  )

(defun run (parts-list garden)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 garden)))
      (2 (format t "~&Part 2: ~a" (p2 garden))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input-lines (uiop:read-file-lines infile-name))
         (data (parse-input input-lines)))
    (run parts data)))

(defun test-small (&rest parts)
  (run parts (parse-input (str:split-omit-nulls #\newline *small-test-input*))))

(defun test-all (&rest parts)
  (dolist (input (list *small-test-input*
                       *med-test-input*
                       *test-input*))
    (run parts (parse-input (str:split-omit-nulls #\newline input)))))

