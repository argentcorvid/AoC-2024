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

(defun quadrantize (bots bounds)
  (let ((centers (mapcar (a:rcurry #'/ 2) bounds)))
    ))

(defun p1 (bots bounds)
  (let ((new-posns (mapcar (a:rcurry #'move-bot 100 bounds))
                   bots))
    (reduce #'* (quadrantize new-posns bounds)))) 

(defun p2 ()
  )

(defun run (parts-list data bounds)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 data)))
      (2 (format t "~&Part 2: ~a" (p2 data))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input-lines (uiop:read-file-string infile-name))
         (data (parse-input input-lines)))
    (run parts data '(101 103))))

(defun test (&rest parts)
  (run parts (parse-input *test-input*) '(11 7)))
