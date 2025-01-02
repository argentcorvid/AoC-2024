;;;day9

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str)))

(eval-when (:compile-toplevel :load-toplevel)
  #+SBCL (add-package-local-nickname 'a 'alexandria)
  #+ECL  (ext:add-package-local-nickname 'a 'alexandria))

(defparameter *day-number* 9)
(defparameter *input-name-template* "2024d~dinput.txt")

(defparameter *test-input*
  "2333133121414131402"
  "p1 checksum: 1928")

(defparameter *simple-input*
  "12345"
  "p1 checksum: 44")


(defun parse-input (input-string)
  (loop for ch across input-string
        for size = (- (char-code ch) 48)
        for idx from 0
        when (and (evenp idx)
                  (plusp size))
            collect (list idx (floor idx 2) size) into used
        when (and (oddp idx)
                  (plusp size))
            collect (list idx size) into free
        finally (return (list used free))))

(defun p1 (drive-map)
  (destructuring-bind (used free)
      drive-map
    (dolist (file (reverse used))
      ))) 

(defun p2 ()
  )

(defun run (parts-list data)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 data)))
      (2 (format t "~&Part 2: ~a" (p2 data))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (data (parse-input uiop:read-file-string infile-name)))
    (run parts data)))

(defun test (&rest parts)
  (run parts (parse-input *test-input*)))
