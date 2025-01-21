;;;2024 day13

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str :cl-ppcre))
  (add-package-local-nickname 'a 'alexandria-2))

(defparameter *day-number* 13)
(defparameter *input-name-template* "2024d~dinput.txt")

(defparameter *test-input*
  "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
")

(defun parse-input (string)
  (let* ((machine-inputs (ppcre:split "\\n\\n" string))
         (button-numbers-regex (ppcre:create-scanner "(?<= [XY])[+-]\\d+"))
         (prize-regex          (ppcre:create-scanner "(?<=[XY]=)\\d+"))
         (out))
    (dolist (machine-info machine-inputs
                          (nreverse out))
      (let ((machine-buttons (mapcar #'parse-integer (ppcre:all-matches-as-strings button-numbers-regex machine-info)))
            (machine-targets (mapcar #'parse-integer (ppcre:all-matches-as-strings prize-regex machine-info))))
        (destructuring-bind (ax ay bx by) machine-buttons
          (destructuring-bind (px py) machine-targets
            (push (list (list ax bx px) (list ay by py)) out)))))))

;;The problem sets up systems of 2 equations with 2 variables (read inputs in columns):
;;
;; A*Xa + B*Xb = Prize X
;; A*Ya + B*Yb = Prize Y
;;
;; solving the first for A:
;; A = (Prize X - B*Xb)
;;     ---------------
;;            Xa
;;
;; Then Substitue for A in the second and solve for B:
;;     (Xa * Prize Y) - (Xb * Prize X)
;; B = -------------------------------
;;          (Xa * Yb) - (Xb * Ya)
;;
;; this is enough to then plug result  using input in to the first equation to get number of A

(defun a-presses (a-x b-x prize-x b-presses)
  (let ((topl prize-x)
        (topr (* b-x b-presses))
        (bot a-x))
    (/ (- topl topr) bot)))

(defun b-presses (a-x a-y b-x b-y prize-x prize-y)
  (let* ((topl (* a-x prize-y))
         (topr (* a-y prize-x))
         (top (- topl topr))
         (bot (- (* a-x b-y) (* b-x a-y))))
    (/ top bot)))

(defun b-presses-from-list (machine-info-list)
  (destructuring-bind ((ax bx px) (ay by py))
      machine-info-list
    (b-presses ax ay bx by px py)))

(defun p1 (machine-info-lists)
  (let ((res 0))
    (dolist (info machine-info-lists res)
      (destructuring-bind ((ax bx px) (ay by py))
          info
        (declare (ignore ay by py))
        (let* ((b-presses (b-presses-from-list info))
               (a-presses (a-presses ax bx px b-presses)))
          (when (and (integerp a-presses)
                     (integerp b-presses)
                     (< b-presses 100)
                     (< a-presses 100))
            (incf res (+ (* 3 a-presses) b-presses)))))))) 

(defun p2 (machine-info-lists)
  (let ((res 0)
        (prize-correction 10000000000000))
    (dolist (info machine-info-lists res)
      (destructuring-bind ((ax bx px) (ay by py))
          info
        (let* ((b-presses (b-presses ax ay bx by (+ px prize-correction) (+ py prize-correction)))
               (a-presses (a-presses ax bx (+ px prize-correction) b-presses)))
          (when (and (integerp a-presses)
                     (integerp b-presses))
            (incf res (+ (* 3 a-presses) b-presses))))))))

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
