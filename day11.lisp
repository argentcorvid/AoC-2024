;;;day11

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str))
  (add-package-local-nickname 'a 'alexandria-2))

(defparameter *day-number* 11)
(defparameter *input-name-template* "2024d~dinput.txt")

(defparameter *short-test-input*
  "0 1 10 99 999"
  "p1: 7 stones after 5 blinks")

(defparameter *test-input*
  "125 17"
  "p1: 55312 stones after 25 blinks")

(defun parse-input (lines)
  (mapcar #'parse-integer (str:split-omit-nulls #\space lines)))

(defun digits (number)
  (declare (type integer number))
  (1+ (floor (log number 10))))

(defun split-digits (number)
  (declare (type integer number))
  (let* ((num-digits (digits number))
         (power      (/ num-digits 2))
         (pow10      (expt 10 power)))
    (declare (type integer num-digits power pow10))
    (multiple-value-call #'list (floor number pow10))))

(defun brute-force-recursive (stones &optional (blinks 0))
  (declare (type fixnum blinks))
  (when (zerop blinks)
    (return-from brute-force-recursive stones))
  (let ((new-stones (mapcan (lambda (stone)
                              (declare (type integer stone))
                              (cond ((zerop stone) (list 1))
                                    ((evenp (digits stone)) (split-digits stone))
                                    (t (list (* stone 2024)))))
                            stones)))
    (brute-force-recursive new-stones (1- blinks)))) 

(defun p1 (stones)
  (length (the list (brute-force-recursive stones 25))))

(defun iterate (stones &optional (blinks 0))
  (flet ((do-blink (stones)
           (mapcan (lambda (stone)
                              (declare (type integer stone))
                              (cond ((zerop stone) (list 1))
                                    ((evenp (digits stone)) (split-digits stone))
                                    (t (list (* stone 2024)))))
                            stones)))
    (do* ((stones (copy-list stones) new-stones)
          (blinks blinks (1- blinks))
          (new-stones))
         ((zerop blinks)
          (length stones))
      (setf new-stones (do-blink stones)))))

(defun p2 (stones)
  ;;  (length (the list (brute-force-recursive stones 75))) ;;runs out of heap, time to iterate!
  (iterate stones 75))

(defun run (parts-list data)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 data)))
      (2 (format t "~&Part 2: ~a" (p2 data))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input-lines (uiop:read-file-string infile-name))
         (data (parse-input input-lines)))
    (run parts data)))

(defun test (&rest parts)
  (run parts (parse-input *test-input*)))
