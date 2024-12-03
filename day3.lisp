;;;day3

(defparameter +day-number+ 3)
(defparameter +working-dir+ (uiop:truenamize "~/aoc-2024/"))
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
        ((null ch) pairs-to-multiply)
      (when (and (char= ch #\m)
                 (string= "ul("
                          (loop repeat 3 collecting (read-char input-stream nil) into str
                                finally (return (coerce str 'string))))
                 (setf pair (loop
                              for ch = (read-character input-stream nil)
                              for i upto 6             ; max 7 characters
                              until (char= ch #\))     ; only until )
                              always (member ch both)  ; abort with nil if not number, comma, or )
                              
                                  )))
      
        (push pair pairs-to-multiply)))))


(defun p1 ()
  ) 

(defun p2 ()
  )

(defun main ()
  (let* ((infile-name (format nil +input-name-template+ +day-number+)))
    (fresh-line)
    (princ "part 1: ")
    (princ (p1 data))
    ;; (fresh-line)
    ;; (princ "part 2: ")
    ;; (princ (p2 data))
    ))
