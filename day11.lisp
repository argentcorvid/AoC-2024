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
  (1+ (floor (log number 10))))

(defun split-digits (number)
  (let* ((num-digits (digits number))
         (power      (/ num-digits 2))
         (pow10      (expt 10 power)))
    (multiple-value-call #'list (floor number pow10))))

(defun p1-brute-force-recursive (stones &optional (blinks 0))
  (when (zerop blinks)
    (return-from p1-brute-force-recursive stones))
  (let ((new-stones (mapcan (lambda (stone)
                              (cond ((zerop stone) (list 1))
                                    ((evenp (digits stone)) (split-digits stone))
                                    (t (list (* stone 2024)))))
                            stones)))
    (p1-brute-force-recursive new-stones (1- blinks)))) 

(defun p1 (stones)
  (length (p1-brute-force-recursive stones 25)))

(defun p2 ()
  )

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
