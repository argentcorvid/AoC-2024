;;;2024 day 15

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:alexandria :str :defclass-std))
  (add-package-local-nickname 'a 'alexandria-2))

(use-package :defclass-std)

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
  (pairlis (list #\^      #\>     #\v     #\<)
           (list #c(-1 0) #c(0 1) #c(1 0) #c(0 -1))))

(defgeneric step-object (obj dir))

(defun parse-input (stream)
  )

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
