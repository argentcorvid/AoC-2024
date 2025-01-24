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

(defparameter test-tree
  (mapcar (lambda (pt)
           (list pt '(0 0)))
          '((4 0) (6 0) (4 3) (6 3) (4 4) (6 4) (3 4) (7 4) (2 4) (8 4) (1 4) (9 4) (0 4) (10 4)
            (1 5) (2 6) (3 7) (4 8) (5 9) (6 8) (7 7) (8 6) (9 5))))

(defun parse-input (lines)
  (let ((scanner (ppcre:create-scanner "(\\d+),(\\d+) v=(-?\\d+),(-?\\d+)"))
        (bots (list)))
    (ppcre:do-register-groups ((#'parse-integer px py vx vy))
        (scanner lines (nreverse bots))
      (push (list (list px py) (list vx vy)) bots))))

(defun move-bot (bot seconds &optional (max-list '(5000 5000)))
  (let* (;(seconds (mod seconds (apply #'* max-list)))
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
    (dolist (bot new-posns quadrant-counts)
      (let ((quad (quadrant-of bot centers)))
        (when quad
          (incf (nth quad quadrant-counts))))))) 

(defun tree-pic? (bots centers)
  (let* ((bot-count (length bots))
         (most-bots (floor bot-count 4))) ;when reached, aound half of the bots are mirrored
    (multiple-value-bind (left-top left-bot right-top right-bot)
        (loop for b in bots
              for q = (quadrant-of b centers)
              when (eql q 0)       ; not odd/even or =, could be nil
                collect b into lt
              when (eql q 2)   
                collect b into lb
              when (eql q 1)
                collect b into rt
              when (eql q 3)
                collect b into rb
              finally (return (values lt lb rt rb)))
      (declare (ignorable right-bot))
      (loop for ltb in left-top 
            counting (find-mirrored ltb right-top centers :h) into mirrored-horizontally
            counting (find-mirrored ltb left-bot centers :v) into mirrored-vertically
            when (or (>= mirrored-vertically most-bots)
                     (>= mirrored-horizontally most-bots))
              return t))))

(defun find-mirrored (bot bot-list mirrors mirror-dir)
  (destructuring-bind ((bot-x bot-y) vel)
        bot
      (declare (ignore vel))
    (let (static-axis
          look-axis
          mirror-at
          mirrored-bot)
      (ccase mirror-dir
        (:h (setf static-axis bot-y
                 look-axis bot-x
                 mirror-at (first mirrors)))
        (:v (setf static-axis bot-x
                 look-axis bot-y
                 mirror-at (second mirrors))))
      (let* ((dist-to-mirror (- mirror-at look-axis))
             (mirrored-pos (+ mirror-at dist-to-mirror))
             (mirrored-bot (if (eql mirror-dir :h)
                               (list mirrored-pos static-axis)
                               (list static-axis mirrored-pos))))
        (find mirrored-bot bot-list :test #'equal)))))

(defun print-bots (bots &optional (bounds '(101 103)) (stream *standard-output*))
  (let* ((array (make-array (reverse bounds) :element-type 'character :initial-element #\.))
         (window (make-array (first bounds) :element-type 'character :displaced-to array)))
    (dolist (bot bots)
      (setf (apply #'aref array (reverse (first bot))) #\*))
    (dotimes (line-offset (second bounds))
      (format stream "~&~a~%" window)
      (setf window (make-array (first bounds) :element-type 'character :displaced-to array :displaced-index-offset (* line-offset (first bounds)))))))

(defun p2 (bots bounds)
  ;; "very rarely, (!)most(!) of the robots should arrange themselves into a picture of a Christmas tree"
  ;;find a picture of a christmas tree
  ;;christmas trees are symmetrical around the x-center
  ;; - OR could be mirrored on y-center
  ;; - same number of points in left and right halves (even numbers)
  ;; - for each y, points on the left would be mirrored on the x-center
  ;; -- for every bot at x - x-center, there should be a bot at x + x-center (? shift center to zero, bots at -x and +x)
  ;; -- -- don't think if you set x-bound to x-center each position will have (at least) 2 bots (don't think it will mirror)?
  ;; how many is "most"?
  ;; god, i hope the picture is centered!
  (let ((bot-count (length bots))
        (centers (mapcar (a:rcurry #'floor 2) bounds)))
    (loop for steps from 0 below (expt 10 9)
          for bots-step = bots then (mapcar (a:rcurry #'move-bot 1 bounds)
                                            bots-step)
          when (= (mod steps (expt 10 4)) 0)
            do (princ #\.)
          when (tree-pic? bots-step centers)
            do (print-bots bots-step)
            and return steps
          finally (return steps ))))

(defun run (parts-list data bounds)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (apply #'* (p1 data 100 bounds))))
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
