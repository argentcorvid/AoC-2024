;;;day6
#+SBCL
(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(str alexandria))
  (add-package-local-nickname 'a 'alexandria))

#+ECL
(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(str alexandria))
  (ext:add-package-local-nickname 'a 'alexandria))

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

(defstruct guard
  (position (list 0 0))
  (direction 'n)
  (steps 0 :type integer)
  (visited (list (list 0 0)))
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
            do (setf guard-start (list line col 'n))
          when (char= cell #\newline)
            do (when (= 0 line)
                 (setf grid-width col))
               (setf col -1)
               (incf line)
               (incf grid-length)
          finally (return (let ((min-max (list (list -1 -1) (list grid-width (1- grid-length))))) ; newline at end of file
                            (make-guard :position guard-start
                                        :visited (list guard-start)
                                        :bounds min-max 
                                        :obstacles (nconc (fence min-max 1)
                                                          (nreverse obstacle-list))))))))

(defun fence (min-max-list &optional (extra 0)) ;making it 1 square bigger will make the total 1 more than needed
  (let ((min-row (- (first (first min-max-list)) extra))
        (min-col (- (second (first min-max-list)) extra))
        (max-row (+ (first (second min-max-list)) extra))
        (max-col (+ (second (second min-max-list)) extra)))
    (remove-duplicates
     (nconc (mapcan (lambda (s)
                      (list (list min-row  s) (list s min-col)))
                    (a:iota (+ 2 max-row extra)
                            :start min-row))
            (mapcan (lambda (s)
                      (list (list max-col s) (list s max-row)))
                    (a:iota (+ 2 max-col extra)
                            :start min-col)))
     :test 'equal)))

(defun oob? (guard) 
  (let* ((pos (guard-position guard))
         (bounds (guard-bounds guard))
         (row (first pos))
         (col (second pos)))
    (or (= row (caar bounds))
        (= row (caadr bounds))
        (= col (cadar bounds))
        (= col (cadadr bounds)))))

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

(defun move-guard (guard next-obstacle)
  (let* ((move-increment (a:assoc-value *directions* (guard-direction guard)))
         (move-distance  (1- (man-dist (guard-position guard) next-obstacle)))
         (next-dir (a:assoc-value *next-dir-lookup* (guard-direction guard)))
         (guard-pos (guard-position guard))
         (cells-travelled (mapcar (lambda (step)
                                   (list (+ (first guard-pos) (* (first move-increment) step))
                                         (+ (second guard-pos) (* (second move-increment) step))
                                         (guard-direction guard)))
                                 (a:iota move-distance :start 1))))
    (when (subsetp cells-travelled (guard-visited guard) :test 'equal) ; loop detection: cells-travelled is subset of the already visited?
      (return-from move-guard))
    (incf (guard-steps guard) move-distance)
    (setf (guard-direction guard) next-dir)
    (mapc (lambda (cell)
            (pushnew cell (guard-visited guard) :test 'equal))
          cells-travelled)
    (setf (guard-position guard) (a:last-elt cells-travelled))))

(defvar *maxloops* (expt 10 6))

(defun p1 (guard)
  (loop for x from 0 upto *maxloops*
        until (oob? guard)
        do (move-guard guard (find-next-obstacle guard))
        finally (return (1- (length (remove-duplicates (guard-visited guard)
                                                       :test 'equal
                                                       :key (lambda (s)
                                                              (butlast s))))))))

(defun p2 (guard) 
  (let ((start-pt (guard-position guard))
        (orig-obst (guard-obstacles guard)))
    (p1 guard) ;collect visited cells
    (let ((orig-visited (guard-visited guard)))
     (loop for cand in orig-visited 
           do (setf (guard-position guard) start-pt
                    (guard-visited guard)  (list (append start-pt '(n)))
                    (guard-obstacles guard) (append (list (butlast cand)) orig-obst))
           counting (loop for x upto *maxloops*
                            thereis (null (move-guard guard (find-next-obstacle guard))) 
                          until (oob? guard)
                          finally (when (>= x *maxloops*)
                                    (error "probable infinite loop not caught")))))))

(defun run (parts-list guard)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 (copy-guard guard))))
      (2 (format t "~&Part 2: ~a" (p2 (copy-guard guard)))))))

(defun main (&rest parts-list)
  (let* ((infile-name (format nil +input-name-template+ +day-number+))
         (input-string (uiop:read-file-string infile-name)))
    (run parts-list (parse-input input-string))))

(defun test (&rest parts-list)
  (run parts-list (parse-input +test-input+)))