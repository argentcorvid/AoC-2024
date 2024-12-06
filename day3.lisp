;;;day3

(defparameter +day-number+ 3)
(defparameter +working-dir+ (uiop:truenamize "~/../aoc-2024/"))
(defparameter +input-name-template+ "2024d~dinput.txt")

(defparameter +test-input+
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
; sum of valid 'mul's is 161. (2*4 + 5*5 + 11*8 + 8*5) 

(defun read-muls (input-stream)
  (let* ((numbers (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
         (seps    (list #\, #\)))
         (both    (append numbers seps)))
    (do ((ch (read-char input-stream nil) (read-char input-stream nil))
         (pairs-to-multiply (list))
         pair)
        ((null ch) (nreverse pairs-to-multiply))
      (when (and (char= ch #\m)
                 (string= "ul("
                          (loop repeat 3 collecting (read-char input-stream nil) into str
                                finally (return (coerce str 'string))))
                 (setf pair (loop
                              for ch = (read-char input-stream nil nil)
                              for i from 1 to 8             ; max 8 characters, including closing )
                              until (char= ch #\))     ; only until )
                               always (member ch both)  ; abort with nil if not number, comma, or )
                              collect ch into bin
                              finally (if (member #\, bin)
                                          (return (mapcar #'parse-integer
                                                          (str:split #\, (coerce bin 'string))))
                                          (return nil)))))
        (if (/= 2 (length pair)) (break))
        (push pair pairs-to-multiply)))))

(defun regex-muls (in-string &key (start 0) (end (length in-string)))
  (let ((mul-scanner (ppcre:create-scanner "(?<=mul\\()([0-9]{1,3}),([0-9]{1,3})(?=\\))"))
        pairs)
    (ppcre:do-register-groups (a b)
        (mul-scanner in-string (nreverse pairs) :start start :end end)
      (push (list (parse-integer a) (parse-integer b)) pairs))))

(defun p1-re (filename)
  (loop for pair in (regex-muls (uiop:read-file-string filename))
        summing (apply #'* pair)))


(defun p1 (input-stream)
  (loop for pair in (read-muls input-stream)
        summing (apply #'* pair)))

(defun p2-re (filename)
  (declare (optimize (speed 0) (debug 3)))
  (let (do-locs
         (do-re (ppcre:create-scanner "(?s)do\\(\\).+?(?:don\\'t\\(\\))")) 
         (input-string (uiop:read-file-string filename))) 
    (setf do-locs (rplaca (ppcre:all-matches do-re input-string) 0))
    ;; (fresh-line)
    ;; (princ do-locs)
    ;; (break)
    (labels ((sum-of-prods (lst)
               (reduce #'+ (mapcar #'(lambda (pair)
                                       (apply #'* pair))
                                   lst))))
      (loop for (start end) on do-locs by #'cddr
            summing (sum-of-prods (regex-muls input-string :start start :end end))))))

(defun main ()
  (let* ((infile-name (format nil +input-name-template+ +day-number+)))
    (fresh-line)
    (princ "part 1: ")
    (with-open-file (data infile-name :direction :input)
      (princ (p1 data))) ; 162633034 too low, 168819032 too high
    (fresh-line)
    (princ "part 1 with regex:")
    (princ  (p1-re infile-name)) ; 1669055464 correct
    ;; (fresh-line)
    ;; (princ "part 2: ")
    ;; (princ (p2 data))
    (fresh-line)
    (princ "part 2 with regex:") ; 52259815 too low
    (princ (p2-re infile-name))
    ))
