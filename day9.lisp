;;;dayx

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str))
  #+SBCL (add-package-local-nickname 'a 'alexandria)
  #+ECL  (ext:add-package-local-nickname 'a 'alexandria))

(defparameter *day-number* x)
(defparameter *input-name-template* "2024d~dinput.txt")

(defparameter *test-input*
  "2333133121414131402")

(defun parse-input (input-string)
  (let ((fat (make-hash-table)))
    (loop for file-id   from 0
          for file-pos  from 0 by 2
          for free-pos  from 1 by 2
          while (< free-pos (length input-string))
          for file-size = (read-from-string input-string nil nil :start file-pos :end (1+ file-pos))
          for free-size = (read-from-string input-string nil nil :start free-pos :end (1+ free-pos))
          
          do (setf (gethash file-id fat)
                   (pairlis '(:file :size :free :moved)
                            (list file-id file-size free-size (list))))
          finally (setf (gethash :numfiles fat) file-id))
    fat))

(defun p1 ()
  ) 

(defun p2 ()
  )

(defun run (parts-list data)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 data)))
      (2 (format t "~&Part 2: ~a" (p2 data))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input (uiop:read-file-string infile-name))
         (data (parse-input input)))
    (run parts data)))

(defun test (&rest parts)
  (run parts (parse-input *test-input*)))
