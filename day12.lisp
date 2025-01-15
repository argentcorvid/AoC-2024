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

(defun same-neighbors-count (grid row col)
  (flet ((oob? (itm)
           (let ((maxs (array-dimensions grid)))
             (or (some #'minusp itm)
                 (some #'>= itm maxs)))))
    (let* ((point (list row col))
           (candidates (remove-if #'oob? (mapcar (lambda (d)
                                                   (mapcar #'+ point d))
                                                 *directions*))))
      (count-if (a:curry #'char-equal (aref grid row col))
                candidates
                :key (a:curry #'apply #'aref grid)))))

(defun p1 (garden)
  (let* ((table-size (+ 10 (* 2 26)))   ; A-Z,a-z,0-9
         (garden-size (array-total-size garden))
         (garden-dims (array-dimensions garden))
         (plot-areas (make-hash-table :size table-size))
         (plot-perimeters (make-hash-table :size table-size))
         (linear-garden (make-array garden-size :displaced-to garden :element-type 'character)))
    (loop for plot across linear-garden
          for index from 0
          for col = (mod index (second garden-dims))
          for row = (floor (- index col) (first garden-dims))
          for sneighbors = (same-neighbors-count garden row col)
          do (incf (gethash plot plot-perimeters 0) (- 4 sneighbors))  ;no, need to multiply perm and area, then add
             (incf (gethash plot plot-areas 0))) ;
    (break) ;
    (loop for area being each hash-value of plot-areas using (hash-key plot-type)
          for perim = (gethash plot-type plot-perimeters)
          summing (* area perim)))) ;this is wrong
 

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

