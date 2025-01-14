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
  (values (make-array (list (length lines) (length (first lines)))
                      :element-type 'character
                      :initial-contents lines)))

(defun p1 (garden)
  (let ((plot-areas (make-hash-table :size (+ 10 (* 2 26))); A-Z,a-z,0-9
                    )
        (linear-garden (make-array (array-total-size garden) :displaced-to garden)))
    (map nil (lambda (chr)
               (incf (gethash chr plot-areas 0)))
         linear-garden)
    (let ((plot-types (a:hash-table-keys)))
      )))
 

(defun p2 (garden plot-types)
  )

(defun run (parts-list garden plot-types)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 garden)))
      (2 (format t "~&Part 2: ~a" (p2 garden))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input-lines (uiop:read-file-lines infile-name))
         (data (parse-input input-lines)))
    (run parts data)))

(defun test-all (&rest parts)
  (princ (with-output-to-string (*standard-output*)
           (dolist (input (list *small-test-input*
                            *med-test-input*
                            *test-input*))
             (run parts (parse-input (str:split-omit-nulls #\newline input))))))
  (terpri))

