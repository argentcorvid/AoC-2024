;;;2024 day 15

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:alexandria :str :defclass-std :serapeum))
  (add-package-local-nickname 'a 'alexandria-2)
  (add-package-local-nickname 's 'serapeum))

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

(defparameter *p2-small-test-input*
  "#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^")

(defparameter *tiny-test-input-yes-1*
  "#####
#...#
#.O.#
#.O.#
#.@.#

^")

(defparameter *tiny-test-input-yes-2*
"##########
##......##
##.[][].##
##..[]..##
##..@...##

^")

(defparameter *tiny-test-input-no-1*
"##########
##..##...##
##.[][].##
##..[]..##
##..@...##

^")

(defparameter *tiny-test-input-no-2*
"##########
####....##
##.[][].##
##..[]..##
##..@...##

^")

(defvar *debug* nil)
(defvar *warehouse-size* (list 0 0))

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

(defclass/std big-crate (crate)
  nil)

(defclass/std big-crate-l (big-crate)
  ((grid-symbol :r :type character :std #\[)))

(defclass/std big-crate-r (big-crate)
  ((grid-symbol :r :type character :std #\])))

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
  (declare (ignore neighbor warehouse))
  nil)

(defmethod step-object ((object movable-grid-object) (neighbor grid-floor) warehouse)
  "swap the positions of a movable object and an empty floor space. returns the object for the floor space"
  (rotatef (slot-value object 'posn) (slot-value neighbor 'posn))
  neighbor)

(defmethod step-object ((object movable-grid-object) (neighbor crate) warehouse)
  "swap the positions of a movable object and a crate, if possible. returns the object of the space the crate moved to, or nil if no move"
  (let* ((dir (- (posn neighbor) (posn object)))
         (obstacles (obstacle-check object dir warehouse))
         (empty (find-if #'floorp obstacles)))
    (when empty
      (let ((first-pos (posn (elt obstacles 0))))
        (map nil (lambda (obj)
                   (when (typep obj 'movable-grid-object)
                     (incf (slot-value obj 'posn) dir)))
             (subseq obstacles 0 (position empty obstacles)))
        (setf (slot-value empty 'posn) first-pos))
      (rotatef (slot-value object 'posn)
               (slot-value empty 'posn))
      (return-from step-object empty))))

(defmethod step-object ((object movable-grid-object) (neighbor big-crate) warehouse)
  "push a big-crate. if vertical, moves both halves, and any other crates in the way if possible. if horizontal, treats them as any other crate."
  (let ((dir (- (posn neighbor) (posn object))))
    (if (= (imagpart (posn object)) (imagpart (posn neighbor))) ; if on same row,
        ;; then horziontal move, recursively push objects into the empty space (keeps correct order of l/r halves)
        (call-next-method)
        ;; else moving vertically. look vertically for at least one empty space between crates and wall.
        (labels ((p2-obstacles (start-object)
                   "Iterative BFS for spaces of interest in the travel direction, up to nearest wall"
                   (do* ((visited-pts (list) (pushnew current-pt visited-pts :test #'equal))
                         (not-visited (list) (delete-if (lambda (itm)
                                                          (member itm visited-pts :test #'equal))
                                                        (neighbor-fn current-pt)))
                         (search-stack (list start-object) (nconc not-visited search-stack))
                         (current-pt start-object (pop search-stack)))
                        ((endp search-stack)
                         (coerce (sort visited-pts
                                       #'<
                                       :key (lambda (j)
                                              (absdist j start-object)))
                                 'vector))))
                 (neighbor-fn (itm)
                   "returns list of 'valid' neighbors if the given item."
                   (unless (wallp itm) 
                     (if (typep itm 'big-crate)
                         (let ((other-half (find (+ (posn itm) (if (typep itm 'big-crate-l)
                                                                   1
                                                                   -1))
                                                 warehouse :key #'posn)))
                           (list (next-in-dir itm) other-half))
                         (list (next-in-dir itm)))))
                 (movable? (itm)
                   (and (typep itm 'movable-object)
                        (floorp (next-in-dir itm))))
                 (next-in-dir (itm)
                   (find (+ (posn itm) dir) warehouse :key #'posn)))
          (let* ((obstacles (p2-obstacles neighbor))
                 (columns (s:assort obstacles :key (a:compose #'realpart #'posn))))
            (when (every (lambda (c)
                           (find-if #'floorp c))
                         columns)
              (s:do-each (column columns) 
                (call-next-method (elt column 0) (elt column 1) column))
              (let ((next (next-in-dir object)))
                (rotatef (slot-value object 'posn)
                         (slot-value next 'posn))
                (return-from step-object next))))))))

(defmethod step-object :before ((object grid-object) (neighbor grid-object) warehouse)
  (when *debug*
    (apply #'dump-wh warehouse *warehouse-size*)
    (break)))

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
                         (setf *warehouse-size* (list col (1+ row)))
                         (make-array (length wh) :element-type 'grid-object
                                                 :initial-contents (nreverse wh)))
                     (if nl?
                         (progn (incf row)
                                (setf col 0))
                         (progn
                           (push (make-instance (case ch
                                                  (#\# 'grid-wall)
                                                  (#\O 'crate)
                                                  (#\[ 'big-crate-l)
                                                  (#\] 'big-crate-r)
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

(defun big-crate-p (grid-obj)
  (typep grid-obj 'big-crate))

(defmethod gps ((crate crate))
  (with-slots (posn) crate
    (+ (* 100 (imagpart posn))
       (realpart posn))))

(defmethod gps ((crate big-crate-l))
  (call-next-method))

(defmethod gps ((obj big-crate-r))
  (declare (ignore obj))
  0)

(defmethod gps ((obj grid-object))
  (declare (ignore obj))
  0)

(defun inflate-warehouse (warehouse)
  (when (find-if #'big-crate-p warehouse)
    (error "Trying to inflate an already-inflated warehouse!"))
  (loop for blk across warehouse
        with new-wh = (make-array (* 2 (length warehouse))
                                  :fill-pointer 0
                                  :element-type 'grid-object
                                  :initial-element (make-instance 'grid-floor))
        do (let* ((pos (posn blk))
                  (new-pos (complex (* 2 (realpart pos))
                                    (imagpart pos))))
             (etypecase blk
               (robot
                (vector-push (make-instance 'robot :posn new-pos) new-wh)
                (vector-push (make-instance 'grid-floor :posn (1+ new-pos)) new-wh))
               ((or grid-wall grid-floor)
                (dotimes (i 2)
                  (vector-push (make-instance (class-of blk) :posn (+ i new-pos)) new-wh)))
               (crate
                (vector-push (make-instance 'big-crate-l :posn new-pos) new-wh)
                (vector-push (make-instance 'big-crate-r :posn (1+ new-pos)) new-wh))))
        finally
           (incf (second *warehouse-size*) (second *warehouse-size*))
           (return new-wh)))

(defun p1 (warehouse commands)
  (loop with robot = (aref warehouse 0)
        for cmd across commands
        for neighbor = (find (+ (posn robot) (dir-lookup cmd)) warehouse :key #'posn)
        when *debug*
          do (format t "~&command: ~a" cmd)
        do (step-object robot neighbor warehouse))
  (reduce #'+ warehouse :key #'gps)) 

(defun run (parts-list warehouse commands)
  (dolist (part (a:ensure-list parts-list))
    (let ((warehouse (copy-seq warehouse))
          (commands  (copy-seq commands)))
      (ccase part
        (1 (format t "~&Part 1: ~a" (p1 warehouse commands)))
        (2
         (setf warehouse (inflate-warehouse warehouse))
         (format t "~&Part 2: ~a" (p1 warehouse  commands))))
      (apply #'dump-wh warehouse *warehouse-size*)
      (terpri))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*)))
    (multiple-value-call #'run parts
      (with-open-file (data infile-name :direction :input)
        (parse-input data)))))

(defun test (&rest parts)
  (dolist (ip (list *small-test-input* *p2-small-test-input* *big-test-input*))
    (multiple-value-call #'run parts
      (with-input-from-string (s ip)
        (parse-input s)))))
