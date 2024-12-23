;;;day7

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str))
  #+SBCL (add-package-local-nickname 'a 'alexandria)
  #+ECL  (ext:add-package-local-nickname 'a 'alexandria))

(defparameter *day-number* 7)
(defparameter *input-name-template* "2024d~dinput.txt")

(defparameter *test-input*
  (str:split #\newline
             "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
"
             :omit-nulls t))

(defparameter *ti2*
  (str:split #\newline "13: 6 7 3
42: 6 7 3
10: 6 7 3
21: 6 7 3
"
             :omit-nulls t))

(defparameter *ti3*
  (str:split #\newline "40656: 26 37 33 56 79 42 60 33
20592: 46 62 81 97 72
39022201: 23 47 69 82 69 83 41
536008016: 73 91 24 78 75 93 11 36 42 86 25 24 66 26
81422384: 57 64 82 84 84 43 17 16
5575106866126011: 94 43 54 52 36 29 26 94 55 84 27 87 97
5172: 66 71 36 25 74 50 91
206506492: 21 31 19 67 87 16 86 69 38 12 13 10
1030269780930: 75 50 16 78 66 41 39 54 11 83 54 34 87
2731294098: 91 27 19 32 27 15 94 18
"
             :omit-nulls t))

                                        ;(defun parse-input (lines))

(defun possible (result number-list &optional (ops (list #'/ #'-)))
  (labels ((%possible (prev nl)
             (when (or (not (plusp prev))
                       (not (integerp prev)))
               (return-from %possible nil))
             (destructuring-bind (a . rest) nl
               (when (endp rest)
                 (return-from %possible (= prev a)))
               (loop for op in ops
                     when (%possible (funcall op prev a)
                                     rest)
                       return t))))
    (%possible result (nreverse number-list))))

(defun endswith (num1 num2)
  (if (zerop num2)
      (zerop (mod num1 10))
      (zerop (expt (mod (- num1 num2) 10)
                   (digits num2)))))
(defun digits (n)
  (1+ (floor (log n 10))))

(defun decat (num1 num2)
  (if (endswith num1 num2)
      (floor num1 (expt 10 (digits num2)))
      -1))

(defun decat-dumb (num1 num2)
  (let* ((n1 (format nil "~d" num1))
         (n2 (format nil "~d" num2))
         (st (- (length n1) (length n2))))
    (if (and (plusp st)
             (string= n1 n2 :start1 st))
        (parse-integer n1 :end st)
        -1)))

(defun p1 (lines)
  (loop for line in lines
        for (result . numbers) = (mapcar (a:rcurry #'parse-integer :junk-allowed t)
                                         (str:split #\space line :omit-nulls t))
        when (possible result numbers)
          sum result)) 

(defun p2 (lines)
  (loop for line in lines
        for (result . numbers) = (mapcar (a:rcurry #'parse-integer :junk-allowed t)
                                         (str:split #\space line :omit-nulls t))
        when (possible result numbers (list #'/ #'decat-dumb #'-))
          sum result))

(defun run (parts-list data)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 data)))
      (2 (format t "~&Part 2: ~a" (p2 data))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input-lines (uiop:read-file-lines infile-name))
        ; (data (parse-input input-lines))
         )
    (run parts input-lines)))

(defun test (&rest parts)
  (run parts *test-input*))
