;;; 2024 day14

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str :cl-ppcre))
  (add-package-local-nickname 'a 'alexandria-2))

(defparameter *day-number* 14)
(defparameter *input-name-template* "2024d~dinput.txt")

(defparameter *test-input*
  "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
"
  "p -> position
v -> velocity")

(defun parse-input (lines)
  (let ((scanner (ppcre:create-scanner "(\\d+),(\\d+) v=(-?\\d+),(-?\\d+)"))
        (bots (list)))
    (ppcre:do-register-groups ((#'parse-integer px py vx vy))
        (scanner lines (nreverse bots))
        (push (list (list px py) (list vx vy)) bots))))

(defun move-bot (bot seconds &optional (max-list '(5000 5000)))
  (let* ((seconds (mod seconds (apply #'* max-list)))
         (new-pos (mapcar (lambda (pc vc mc)
                            (mod (+ pc (* vc seconds))
                                 mc))
                          (first bot)
                          (second bot)
                          max-list)))
    (list new-pos (second bot))))

(defun quadrant-of (bot centers)
  (destructuring-bind ((px py) vel)
          bot
        (declare (ignore vel))
        (destructuring-bind (x-center y-center)
            centers
          (let ((left (< px x-center))
                (top  (< py y-center))
                (right (> px x-center))
                (bottom (> py y-center)))
           (cond ((and left top)     0)
                 ((and right top)    1)
                 ((and left bottom)  2)
                 ((and right bottom) 3)
                 (t nil))))))

(defun p1 (bots seconds bounds)
  (let ((new-posns (mapcar (a:rcurry #'move-bot seconds bounds)
                           bots))
        (centers (mapcar (a:rcurry #'floor 2) bounds))
        (quadrant-counts (make-list 4 :initial-element 0)))
    (dolist (bot new-posns
                 (apply #'* quadrant-counts))
      (let ((quad (quadrant-of bot centers)))
        (when quad
          (incf (nth quad quadrant-counts))))))) 

(defun p2 (bots bounds)
  ;; "very rarely, (!)most(!) of the robots should arrange themselves into a picture of a Christmas tree"
  ;;find a picture of a christmas tree
  ;;christmas trees are symmetrical around the x-center
  ;; - same number of points in left and right halves (even numbers)
  ;; - for each y, points on the left would be mirrored on the x-center
  ;; -- for every bot at x - x-center, there should be a bot at x + x-center (? shift center to zero, bots at -x and +x)
  ;; -- -- don't think if you set x-bound to x-center each position will have (at least) 2 bots (don't think it will mirror)?
  ;; how many is "most"?
  )

(defun run (parts-list data bounds)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 data 100 bounds)))
      (2 (format t "~&Part 2: ~a" (p2 data bounds))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input-lines (uiop:read-file-string infile-name))
         (data (parse-input input-lines)))
    (run parts data '(101 103))))

(defun test (&rest parts)
  (when (member 2 parts)
      (format t "~&Part 2 undefined with test input for this day"))
  (run '(1) (parse-input *test-input*) '(11 7)))
