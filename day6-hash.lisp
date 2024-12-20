;;;day6
#+SBCL
(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(str alexandria))
  (add-package-local-nickname 'a 'alexandria))

#+ECL
(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(str alexandria))
  (ext:add-package-local-nickname 'a 'alexandria))

(declaim (optimize (debug 3) ))

(defparameter +day-number+ 6)
(defparameter +input-name-template+ "2024d~dinput.txt")

(defparameter +test-input+
  "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
")

(defvar *p2-test-answer-locations*
  '((6 3) (7 6) (7 7) (8 2) (8 3) (9 7))
  "what the answer for P2 with test input should be")

(defparameter *p2-my-answers*
  '((9 7) (7 7) (8 1) (8 3) (7 6) (6 3))
  "what my code returns")

(defstruct guard
  (position (list 0 0))
  (direction 'n)
  (steps 0 :type integer)
  (visited (make-hash-table :test 'equal))
  (bounds (list (list 0 0) (list 0 0)))
  (obstacles (list)))

(defvar *directions* (pairlis '(n      e     s     w)
                              '((-1 0) (0 1) (1 0) (0 -1))))

(defun parse-input (input-string)
  (let ((obstacle-list (list))
        guard-start)
    (loop with grid-width = 1
          with grid-length = 1
          with line = 0
          for col from 0
          for cell across input-string
          when (char= cell #\#)
            do (push (list line col) obstacle-list)
          when (char= cell #\^)
            do (setf guard-start (list line col))
          when (char= cell #\newline)
            do (when (= 0 line)
                 (setf grid-width col))
               (setf col -1)
               (incf line)
               (incf grid-length)
          finally (return (let* ((min-max (list (list -1 -1) (list grid-width (1- grid-length)))) ; newline at end of file
                                 (g (make-guard :position guard-start
                                        ;:visited (list guard-start)
                                              :bounds min-max 
                                              :obstacles (nconc (fence min-max 1)
                                                                (nreverse obstacle-list)))))
                            (setf (gethash guard-start (guard-visited g)) 'n)
                            g)))))

(defun fence (min-max-list &optional (extra 0)) ;making it 1 square bigger will make the total 1 more than needed
  (destructuring-bind ((rmin cmin) (rmax cmax))
      min-max-list
    (let ((min-row (- rmin extra))
          (min-col (- cmin extra))
          (max-row (+ rmax extra))
          (max-col (+ cmax extra)))
      (remove-duplicates
       (nconc (mapcan (lambda (s)
                        (list (list min-row  s) (list s min-col)))
                      (a:iota (+ 2 max-row extra)
                              :start min-row))
              (mapcan (lambda (s)
                        (list (list max-col s) (list s max-row)))
                      (a:iota (+ 2 max-col extra)
                              :start min-col)))
       :test 'equal))))

(defun oob? (guard) 
  (let* ((pos (guard-position guard))
         (row (first pos))
         (col (second pos)))
    (destructuring-bind ((rmin cmin) (rmax cmax))
        (guard-bounds guard)
      (or (= row rmin)
          (= row rmax)
          (= col cmin)
          (= col cmax)))))

(defun man-dist (pt1 pt2)
  "find the 'manhattan' distance between 2 points"
    ;; (reduce #'+ (mapcar (lambda (a b)
    ;;                       (abs (- a b)))
    ;;              pt1 pt2)) ; conses a lot
  (+ (abs (- (first pt1) (first pt2))) (abs (- (second pt1) (second pt2)))))

(defun find-next-obstacle (guard)
  (let* ((elf-pos (guard-position guard))
         (elf-dir (a:assoc-value *directions* (guard-direction guard)))
         (candidates (remove-if-not (lambda (obs-pos)
                                      (and (or (= (first elf-pos)  ;in the same row or column
                                                  (first obs-pos))
                                               (= (second elf-pos)
                                                  (second obs-pos)))
                                           (equal (mapcar (lambda (o e) ;in the same direction guard is pointing
                                                            (signum (- o e)))
                                                          obs-pos elf-pos)
                                                  elf-dir)))
                                    (guard-obstacles guard))))
    (loop for c in candidates
          with min-c = (mapcar #'* ;something that is far away max row,col in bounds times dir?
                               (second (guard-bounds guard))
                               elf-dir) 
          when (< (man-dist elf-pos c)
                  (man-dist elf-pos min-c))
            do (setf min-c c)
          end
          finally (return min-c))))

(defvar *next-dir-lookup* (pairlis '(n s e w) '(e w s n)))

(defun filter-oob (position-list guard)
  (remove-if-not (lambda (itm)
                   (destructuring-bind (r c d) itm
                     (declare (ignore d))
                     (destructuring-bind ((rmin cmin) (rmax cmax)) (guard-bounds guard)
                       (and (< rmin r rmax)
                            (< cmin c cmax)))))
                 position-list))

(defun move-guard (guard next-obstacle)
  (let* ((move-increment (a:assoc-value *directions* (guard-direction guard)))
         (move-distance  (1- (man-dist (guard-position guard) next-obstacle)))
         (next-dir (a:assoc-value *next-dir-lookup* (guard-direction guard)))
         (guard-pos (guard-position guard))
         (cells-travelled (mapcar (lambda (step)
                                    (list (+ (first guard-pos) (* (first move-increment) step))
                                          (+ (second guard-pos) (* (second move-increment) step))))
                                  (a:iota move-distance :start 1))))
    (incf (guard-steps guard) move-distance)
    (when (and
           (< 0 move-distance)
           (loop for c in cells-travelled
                   thereis (member next-dir (gethash c (guard-visited guard))
                                   :test #'equal))) ; loop detection: cells-travelled is subset of the already visited?
      ;(break)
      (return-from move-guard))
    (setf (guard-direction guard) next-dir)
    (mapc (lambda (cell)
            ;(pushnew cell (guard-visited guard) :test 'equal)
            (pushnew next-dir (gethash cell (guard-visited guard))))
          cells-travelled)
    (if cells-travelled
        (setf (guard-position guard) (a:last-elt cells-travelled)))))

(defvar *maxloops* (expt 10 6))

(defun p1 (guard)
  (loop for x from 0 upto *maxloops*
        until (oob? guard)
        do (move-guard guard (find-next-obstacle guard))
        finally (return (guard-visited guard))))

(defun p2 (guard) 
  (let* ((start-pt (guard-position guard))
         (orig-obst (guard-obstacles guard)))
    (p1 guard) ;collect visited cells
    (let ((orig-visited (filter-oob (a:hash-table-keys (guard-visited guard)) guard)))
     (loop with res = (make-hash :test 'equal)
           for cand in orig-visited
           do (setf (guard-position guard) start-pt
                    
                    (guard-obstacles guard) (append (list cand) orig-obst)
                    (guard-steps guard) 0
                    (guard-direction guard) 'n)
              (clrhash (guard-visited guard))
              (setf (gethash start-pt (guard-visited guard)) 'n)
           when (loop for x upto *maxloops*
                        thereis (null (move-guard guard (find-next-obstacle guard))) 
                      until (oob? guard)
                      finally (when (>= x *maxloops*)
                                (error "probable infinite loop not caught"))
                              (return nil))
             do (setf (gethash cand res) t)
           finally (return res)))))

(defun run (parts-list guard)
  (dolist (part (a:ensure-list parts-list) )
    (ccase part
      (1 (let* ((ng (copy-guard guard))
                (results (hash-table-count (p1 ng))))
           (format t "~&Part 1: ~a" (1- results))
          ; (format t "~&visited cells: ~a" results)
           ))
      (2 (let* ((ng (copy-guard guard))
                (results (hash-table-count (p2 ng))))
       ;    (format t "~&loop obstacle locations: ~a" results)
           (format t "~&Part 2: ~a" results))))))

(defun main (&rest parts-list)
  (let* ((infile-name (format nil +input-name-template+ +day-number+))
         (input-string (uiop:read-file-string infile-name)))
    (run parts-list (parse-input input-string))))

(defun test (&rest parts-list)
  (run parts-list (parse-input +test-input+)))
