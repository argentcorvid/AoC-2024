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
            collect (list idx nil size nil) into free
        finally (return (list used free))))

(defun not-full (itm)
  (or (null (fourth itm))
      (> (third itm) (used-space itm))))

(defun used-space (free-block)
  (reduce #'+ (mapcar #'a:lastcar (fourth free-block))))

(defun ft-flatten (ft)
  (loop for (seq id size moved) in ft
        unless (null id)
          collect (list seq id size)
        else
          append (loop for (orig-seq id size) in moved
                       collect (list seq id size))))

(defun p1 (fileblocks)
  (declare (optimize (speed 0) (debug 3)))
  (do* ((used (reverse (first fileblocks)))
        (stopl (floor (length used) 2))
        (free (copy-list (second fileblocks)))
        (current-file (pop used) (pop used))
        (current-free (first free) (find-if #'not-full free))
        (ft-out (list)))
       ((or (<= (length used) stopl)
            (null current-free))
        (push current-file used)
        (sort (append ft-out used) #'< :key #'car))
    (let ((file-size (third current-file))
          (free-size (- (third current-free) (used-space current-free)))) ;;need to subtract any moved in here
      (if (< free-size file-size)
          (destructuring-bind (file remainder)
              (split current-file free-size) ;;split file
            (a:appendf (fourth current-free) (list file)) ;;move correct part to free
            (push current-free ft-out) ;;done with that 'free' block, collect it
            (push remainder used) ;;end-if, go through the loop again and possibly split the file more
            ) 
          
          (progn ;;else move file into free
            (a:appendf (fourth current-free) (list current-file))
            (push current-free ft-out))))))

(defun split (fb needed-size)
  (destructuring-bind (pos id size) fb
    (assert (<= needed-size size) (needed-size)
            "requested size ~a is too large for file block with size ~a" needed-size size)
    `((,pos ,id ,needed-size) (,pos ,id ,(- size needed-size)))))

(defun p2 ()
  )

(defun run (parts-list data)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 data)))
      (2 (format t "~&Part 2: ~a" (p2 data))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*)))
    (run parts (parse-input (uiop:read-file-string infile-name)))))

(defun test (&rest parts)
  (run parts (parse-input *test-input*)))
