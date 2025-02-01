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

(defvar valid-commands
  (list #\^      #\>     #\v     #\<))

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

(defgeneric step-object (obj dir) ;; :'( methods specialize on CLASS not TYPE 
  (:documentation "move an object one step in the given direction")
  (:method ((obj grid-object) towards)
    "move the object one step towards the point given, uses signum of each part separately to ensure only full steps"
    (when (robot-command-p towards)
      (setf towards (dir-lookup towards)))
    (check-type towards grid-direction)
    (if (fixed? obj)
        (posn obj)
        (incf (slot-value obj 'posn) (let ((r (signum (realpart towards)))
                                           (c (signum (imagpart towards))))
                                       (complex r c)))))
  (:method ((obj crate) towards)
    "checks in the given direction to see if blocked or more boxes. moves if possible")
  (:method ((obj robot) towards)
    "checks in the given direction to see if we are blocked and if we can push boxes, then moves"))

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
                              (#\. 'grid-floor)
                              (#\@ 'robot))
                            :posn (complex col row))
             warehouse)
            (incf col))))
    (do* ((ch (read-char stream)
              (read-char stream nil nil)))
         ((null ch))
      (when (graphic-char-p ch)
        (vector-push-extend ch commands )))
    (values warehouse
            commands)))

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
