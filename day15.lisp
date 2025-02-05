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

(defvar *debug* nil)

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

(class/std grid-object posn :ri :type grid-point :std #c(0 0))

(class/std fixed-object fixed? :r :std t)

(class/std movable-object fixed? :r :std nil)

(defclass/std fixed-grid-object (grid-object fixed-object) ())

(defclass/std grid-wall (fixed-grid-object)
  ((grid-symbol :r :type character :std #\#)))

(defclass/std grid-floor (fixed-grid-object)
  ((solid? :r :std nil)
   (grid-symbol :r :type character :std #\.)))

(defclass/std movable-grid-object (grid-object movable-object)
  ((solid? :r :std t)))

(defclass/std robot (movable-grid-object)
  ((grid-symbol :r :type character :std #\@)))

(defclass/std crate (movable-grid-object)
  ((grid-symbol :r :type character :std #\O)))

(defparameter *directions*
  (pairlis valid-commands
           (list #c(0 -1) #c(1 0) #c(0 1) #c(-1 0)))) ;#C(col,row)

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
                                    (and (= (signum (dist object cand))
                                            direction)
                                         (or (wallp cand)
                                             (cratep cand)
                                             (floorp cand))))
                                  warehouse))
         (closest-wall (loop for w across (remove-if-not #'wallp all-obst)
                             for wd = (absdist object w)
                             with mind = (length warehouse)
                             with closest-wall
                             when (< wd mind)
                               do (setf mind wd
                                        closest-wall w)
                             finally (return closest-wall))))
    (sort (remove-if (lambda (obst)
                       (< (absdist object closest-wall) (absdist object obst)))
                     all-obst)
          #'<
          :key (a:curry #'absdist object))))

(defmethod step-object ((object grid-object) (neighbor grid-wall) warehouse) ; don't think this ever gets called? 
  (declare (ignore object neighbor warehouse))
  nil)

(defmethod step-object ((object movable-grid-object) (neighbor grid-floor) warehouse)
  (rotatef (slot-value object 'posn) (slot-value neighbor 'posn))
  neighbor)

(defmethod step-object ((object grid-object) (neighbor crate) warehouse)
  (let* ((dir (- (posn neighbor) (posn object)))
         (obstacles (obstacle-check object dir warehouse))
         (empty (position-if #'floorp obstacles)))
    (when empty
      (let ((next (step-object neighbor (aref obstacles 1) obstacles))
            (op (posn object)))
        (rotatef (slot-value object 'posn) (slot-value next 'posn))
        (return-from step-object next)))))

(defmethod step-object :before ((object grid-object) (neighbor grid-object) warehouse)
  (declare (special *debug*))
  (when (and (boundp *debug*)
             *debug*)
    (dump-wh warehouse 8 8) ;only for small test, obviously
    (break))
 ; (call-next-method)
  )

(defun dump-wh (warehouse rows cols)
  (let ((2dwh (make-array (list rows cols))))
    (loop for cell across warehouse
          for p = (posn cell)
          do (setf (aref 2dwh (imagpart p) (realpart p)) (grid-symbol cell)))
    (fresh-line)
    (loop for idx from 0 below (array-total-size 2dwh)
          when (zerop (mod idx cols))
            do (terpri)
          do (princ (row-major-aref 2dwh idx)))
    (terpri)))

(defun parse-input (stream)
  (let ((warehouse (do* ((wh (list))
                         (ch (read-char stream)
                             (read-char stream nil))
                         (col 0)
                         (row 0)
                         (nl? nil (char= ch #\newline)))
                        ((and nl? (char= #\newline (peek-char nil stream)))
                         (make-array (length wh) :element-type 'grid-object
                                                 :initial-contents (nreverse wh)))
                     (if nl?
                         (progn (incf row)
                                (setf col 0))
                         (progn
                           (push (make-instance (case ch
                                                  (#\# 'grid-wall)
                                                  (#\O 'crate)
                                                  (#\. 'grid-floor)
                                                  (#\@ 'robot))
                                                :posn (complex col row))
                                 wh)
                           (incf col)))))
        (commands (do* ((ch (read-char stream)
                            (read-char stream nil nil))
                        (cmds (list)))
                       ((null ch)
                        (make-array (length cmds) :element-type 'character
                                                  :initial-contents (nreverse cmds)))
                    (when (graphic-char-p ch)
                      (push ch cmds )))))
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

(defmethod gps ((crate crate))
  (with-slots (posn) crate
    (+ (* 100 (imagpart posn))
       (realpart posn))))

(defmethod gps ((obj grid-object))
  (declare (ignore obj))
  0)

(defun p1 (warehouse commands)
  (declare (special *debug*))
  (catch 'abort
    (loop with robot = (aref warehouse 0)
          for cmd across commands
          for neighbor = (find (+ (posn robot) (dir-lookup cmd)) warehouse :key #'posn)
          when *debug* do (format t "~&command: ~a" cmd)
            do (step-object robot neighbor warehouse))
    (reduce #'+ warehouse :key #'gps))) 

(defun p2 (warehouse commands)
  (declare (special *debug*))
  (catch 'abort
    ))

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
