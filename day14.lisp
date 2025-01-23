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

(defun move-bot (bot seconds &optional (x-max 101) (y-max 103))
  (destructuring-bind ((pos-x pos-y) (vel-x vel-y))
      bot
    (let ((new-x (mod (+ pos-x (* vel-x seconds))
                      x-max))
          (new-y (mod (+ pos-y (* vel-y seconds))
                      y-max)))
      (list (list new-x new-y) (second bot)))))

(defun p1 ()
  ) 

(defun p2 ()
  )

(defun run (parts-list data)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 data)))
      (2 (format t "~&Part 2: ~a" (p2 data))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input-lines (uiop:read-file-lines infile-name))
         (data (parse-input input-lines)))
    (run parts data)))

(defun test (&rest parts)
  (run parts (parse-input *test-input*)))
