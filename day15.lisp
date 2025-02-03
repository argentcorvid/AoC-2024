;;;2024 day 15

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:alexandria :str :defclass-std))
  (add-package-local-nickname 'a 'alexandria-2))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :defclass-std))

(defparameter *day-number* 15)
(defparameter *input-name-template* "2024d~dinput.txt")

(defparameter *big-test-input*
  "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")

(defparameter *small-test-input*
  "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<")



(deftype grid-point ()
  '(or (integer 0 *)
    (complex (integer 0 *))))

(deftype grid-direction ()
  '(or (integer) (complex integer)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter valid-commands
    (list #\^      #\>     #\v     #\<)))

(deftype robot-command ()
  `(and character (member ,@valid-commands)))

(defun robot-command-p (obj)
  (typep obj 'robot-command))

(defun grid-direction-p (obj)
  (typep obj 'grid-direction))

(defun grid-point-p (obj)
  (typep obj 'grid-point))

(defclass/std grid-object nil
  ((posn :ri :type grid-point :std #c(0 0))
   (fixed? solid? :r :std t)))

(defclass/std grid-wall (grid-object)
  ((grid-symbol :r :type character :std #\#)))

(defclass/std grid-floor (grid-object)
  ((solid? :r :std nil)
   (grid-symbol :r :type character :std #\.)))

(defclass/std movable-grid-object (grid-object)
  ((fixed? :r :std nil)))

(defclass/std robot (movable-grid-object)
  ((grid-symbol :r :type character :std #\@)))

(defclass/std crate (movable-grid-object)
  ((grid-symbol :r :type character :std #\O)))

(defparameter *directions*
  (pairlis valid-commands
           (list #c(-1 0) #c(0 1) #c(1 0) #c(0 -1))))

(defun dir-lookup (arrow-char)
  (check-type arrow-char robot-command)
  (cdr (assoc arrow-char *directions*)))

(defun dist (obj-1 obj-2)
  (- (posn obj-2) (posn obj-1)))

(defun absdist (obj-1 obj-2)
  (abs (dist obj-1 obj-2)))

(defun obstacle-check (object direction warehouse)
  "look in the given direction for objects that might get in the way, up to closest wall"
  (when (robot-command-p direction)
    (setf direction (dir-lookup direction)))
  (check-type direction grid-direction)
  (let* ((obj-pos  (posn object))
         (all-obst (remove-if-not (lambda (cand)
                                    (and (= (signum (dist obj cand))
                                            direction)
                                         (or (wallp cand)
                                             (cratep cand))))
                                  warehouse))
         (walls-only (remove-if-not #'wallp all-obst))
         (closest-wall (loop for w across walls-only
                             for wd = (absdist obj w)
                             with mind = (length warehouse)
                             with closest-wall
                             when (< wd mind)
                               do (setf mind wd
                                        closest-wall w)
                             finally (return closest-wall))))
    (remove-if (lambda (obst)
                 (< (absdist obj closest-wall) (absdist obj obst)))
               all-obst)))


(defun move (object direction)
  (if (fixed? object)
      (posn object)
      (incf (slot-value object 'posn)
            (let ((r (signum (realpart direction)))
                  (c (signum (imagpart direction))))
              (complex r c)))))

(defun parse-input (stream)
  (let ((warehouse (make-array 1 :adjustable t
                                 :fill-pointer 0
                                 :element-type 'grid-object
                                 :initial-element (make-instance 'grid-wall)))
        (commands (make-array 1 :adjustable t
                                :fill-pointer 0
                                :element-type 'character
                                :initial-element #\<)))
    (do* ((ch (read-char stream)
              (read-char stream nil))
          (col 0)
          (row 0)
          (nl? nil (char= ch #\newline)))
         ((and nl? (char= #\newline (peek-char nil stream))))
      (if nl?
          (progn (incf row)
                 (setf col 0))
          (progn
            (vector-push-extend
             (make-instance (case ch
                              (#\# 'grid-wall)
                              (#\O 'crate)
                             ; (#\. 'grid-floor)
                              (#\@ 'robot))
                            :posn (complex col row))
             warehouse)
            (incf col))))
    (do* ((ch (read-char stream)
              (read-char stream nil nil)))
         ((null ch))
      (when (graphic-char-p ch)
        (vector-push-extend ch commands )))
    (rotatef (aref warehouse 0)
             (aref warehouse (position-if #'robotp warehouse)))
    (values warehouse commands)))

(defun robotp (grid-obj) ;need?
  (typep grid-obj 'robot))

(defun cratep (grid-obj)
  (typep grid-obj 'crate))

(defun wallp (grid-obj)
  (typep grid-obj 'grid-wall))

(defun floorp (grid-obj)
  (typep grid-obj 'grid-floor))

(defun p1 (warehouse commands)
  (loop with robot = (aref warehouse 0)
        for cmd across commands
        do (step-obj robot command))
  (reduce #'gps (remove-if-not #'cratep warehouse))) 

(defun p2 (warehouse commands)
  )

(defun run (parts-list warehouse commands)
  (dolist (part (a:ensure-list parts-list))
    (let ((warehouse (copy-seq warehouse))
          (commands  (copy-seq commands)))
     (ccase part
       (1 (format t "~&Part 1: ~a" (p1 warehouse commands)))
       (2 (format t "~&Part 2: ~a" (p2 warehouse commands)))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*)))
    (multiple-value-call #'run parts
      (with-open-file (data infile-name :direction :input)
        (parse-input data)))))

(defun test (&rest parts)
  (run parts (parse-input *test-input*)))
