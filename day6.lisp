;;;day6

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
......#...")

(defstruct guard
  (position (list 0 0))
  (direction 'n)
  (steps 0 :type integer)
  (visited (list (list 0 0)))
  (bounds (list (list 0 0) (list 0 0))))

(defvar *directions* (pairlis '(n      e     s     w)
                              '((-1 0) (0 1) (1 0) (0 -1))))

(defun move-guard ()
  )

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
          finally (return (values (make-guard :position guard-start
                                              :visited (list guard-start)
                                              :bounds (list (list -1 -1) (list grid-width grid-length)))
                                  (nreverse obstacle-list))))))

(defun p1 (guard obstacles)
  ) 

(defun p2 (guard obstacles)
  )

(defun run (parts-list guard obstacles)
  (unless (consp parts-list)
    (setf parts-list (list parts-list)))
  (dolist (part parts-list)
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 guard obstacles)))
      (2 (format t "~&Part 2: ~a" (p2 guard obstacles))))))

(defun main (parts-list)
  (let* ((infile-name (format nil +input-name-template+ +day-number+))
         (input-string (uiop:read-file-string infile-name)))
    (multiple-value-call #'run parts-list (parse-input input-string))))

(defun test (parts-list)
  (multiple-value-call #'run parts-list (parse-input +test-data+)))
