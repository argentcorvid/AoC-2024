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
EEEC
"
  "part 1: 5 regions, total price 140
part 2: 5 regions:
  A,B,D,E: 4 sides
  C: 8 sides
total price: 80")

(defparameter *med-test-input*
  "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
"
  "part 1: 5 regions, total price 772
part 2: total price 436")

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
MMMISSJEEE
"
  "part 1: 11 regions, total price 1930
part 2: total price 1206")

(defparameter *pt2-test1*
  "EEEEE
EXXXX
EEEEE
EXXXX
EEEEE
"
  "part 2: total price 236
  E region: Area 17, Sides 12")

(defparameter *pt-test2*
  "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA
"
  "part 2: total price 368
B regions are NOT connected")

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
      (remove-if (a:curry #'char/= (aref grid row col))
                 candidates
                 :key #'keyfun))))

(defun complex-aref (array complex-point &optional (ref-point #c(0 0)))
  (let ((point (+ complex-point ref-point)))
    (aref array (realpart point) (imagpart point))))

(defparameter *corner-looks*
  (loop for look-dir = '(#c (0 1) #c (1 0) #c (1 1))
          then (mapcar (a:curry #'* #c (0 -1)) look-dir)
        repeat 4
        collect look-dir))

(defun point-corners (grid point)
  (loop with cpoint = (apply #'complex point)
        with plot-type = (apply #'aref grid point)
        for look-dir in *corner-looks*
        ;;for look-points = (mapcar (a:curry #'+ cpoint) look-dir)
        for (e s se) = (mapcar (lambda (pt)
                                 (handler-case (complex-aref grid pt cpoint)
                                   (error () #\$)))
                               look-dir)
        counting (or (and (char= e s plot-type)
                          (char/= se plot-type))
                     (and (char/= plot-type s)
                          (char/= plot-type e)))))

(defun find-region-from-point (grid row col)
  " floodfill to fimd adjacent plots"
  (do* ((visited (list) (cons current-point visited))
        (current-point (list row col) (pop search-queue))
        
        (neighbors (same-neighbors grid row col)
                   (apply #'same-neighbors grid current-point))
        (perimeter (- 4 (length neighbors))
                   (- 4 (length neighbors)))
        (corners (point-corners grid current-point)
                (point-corners grid current-point))
        (not-visited neighbors
                     (set-difference neighbors visited :test (lambda (n v) (equal n (car v)))))
        (search-queue not-visited (union not-visited search-queue :test #'equal)))
       ((endp search-queue)
        (pushnew (list current-point perimeter corners) visited :key #'car :test #'equal)
        (nreverse visited))
    (setf current-point (list current-point perimeter corners))
    ;;(break)
    ))

(defun collect-regions (garden)
  (let* ((garden-size (array-total-size garden))
         (garden-dims (array-dimensions garden))
         (linear-garden (make-array garden-size :displaced-to garden
                                                :element-type 'character)))
    (loop with visited-marker = #\$
          with (nil cols) = garden-dims
          for prev-start-idx = 0 then next-start-idx
          for next-start-idx = (position visited-marker linear-garden
                                         :test-not #'char=
                                         :start prev-start-idx)
          until (null next-start-idx)
          for col = (mod next-start-idx cols)
          for row = (floor (- next-start-idx col) cols)
          for region = (find-region-from-point garden row col)
          collecting region
          do (loop for (pt . nil) in region
                   do (setf (apply #'aref garden pt) visited-marker)))))

(defun p1 (garden)
  (let* ((garden (a:copy-array garden)))
    (loop for region in (collect-regions garden)
          sum (loop for (pt perim) in region
                    count pt into area
                    sum perim into perimeter
                    finally (return (* area perimeter))))))
 

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

