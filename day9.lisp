;;;day9

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str :defclass-std))
  (import 'defclass-std:defclass/std)
  #+SBCL (add-package-local-nickname 'a 'alexandria)
  #+ECL  (ext:add-package-local-nickname 'a 'alexandria))

(defparameter *day-number* 9)
(defparameter *input-name-template* "2024d~dinput.txt")

(defparameter *test-input*
  "2333133121414131402"
  "checksum: 1928")

(defparameter *simple-input*
  "12345"
  "checksum: 44")

(defclass/std fileblock nil
  ((file-id
    initial-location
    current-location
    block-length
    :type fixnum :std 0)))

(defun next-freespace (ftable &optional (start 0) (end (hash-table-count ftable))) ;need better end conditions
  (loop for idx from start below end
        for fb = (gethash idx ftable)
        when (and (not (null fb))
                  (minusp (file-id fb)))
          return (values fb (block-length fb))))

(defun next-filespace (ftable &optional (start (hash-table-count ftable)) (end 0))
  (loop for idx downfrom start to end
        for fb = (gethash idx ftable)
        when (and (not (null fb))
                  (not (minusp (file-id fb))))
          return (values fb (block-length fb))))

(defun split-fileblock (fb size)
  (let* ((old-size (block-length fb))
         (new-loc  (+ 1 size (current-location fb))))
    (loop
      (when (<= size old-size)
        (return))
      (cerror "enter a new size" "requested size ~a is too big for fb ~a size ~a" size fb old-size)
      (format t "~&New size: ")
      (setq size (read))
      (fresh-line))
    (decf (block-length fb) size)
    (make-instance 'fileblock
                   :file-id (file-id fb)
                   :initial-location new-loc
                   :current-location new-loc
                   :block-length size)))

(defun swap-fileblocks (ftable src-idx dst-idx) ;need to handle if the src is smaller than the freespace
  (let ((src-block (gethash src-idx ftable))
        (dst-block (gethash dst-idx ftable)))
    (if (= (block-length dst-block) (block-length src-block))
        (setf (current-location src-block) dst-idx
              (current-location dst-block) src-idx
              (gethash dst-idx ftable) src-block
              (gethash src-idx ftable) dst-block)
        (error "Not enough room to move block ~a to block ~a" src-block dst-block))))

(defun parse-input (input-string &optional (block-length 1))
  (loop with fat = (make-hash-table :size (* 2 (length input-string)))
        with skip = block-length
        for file-id   from 0
        for file-str-pos  from 0 by skip
        while (< file-str-pos (length input-string))
        for file-size = (read-from-string input-string nil nil :start file-str-pos :end (+ block-length file-str-pos))
        for file-pos = 0 then (with-accessors ((loc initial-location) (len block-length))
                                  (gethash (1- file-str-pos) fat)
                                (+ len loc))
        do (setf (gethash file-str-pos fat)
                 (make-instance 'fileblock :file-id (multiple-value-bind
                                                          (q r)
                                                        (floor file-id 2)
                                                      (if (zerop r)
                                                          q
                                                          -1))
                                           :initial-location file-pos ;no, need expanded posns
                                           :current-location file-pos
                                           :block-length file-size))
        finally (return fat)))

(defun checksum (ftable)
  (let ((sum 0))
    (maphash (lambda (pos blk)
               (declare (ignore pos))
               (with-accessors ((fid file-id)
                                (loc current-location)
                                (len block-length))
                   blk
                 (unless (minusp fid)
                   (incf sum (loop for l from loc
                                   repeat len
                                   summing (* fid l))) ;need to accout for runs here
                   )))
             ftable)
    sum))

(defun p1 (ftable)
  (do* ((max-idx (1- (hash-table-count ftable)) (- max-idx 2))
        (min-idx 1 (+ min-idx 2))
        (src-blk (next-filespace ftable max-idx)
                 (next-filespace ftable max-idx))
        (dst-blk (next-freespace ftable min-idx)
                 (next-freespace ftable min-idx))
        (diff (- (block-length src-blk) (block-length dst-blk) )
              (- (block-length src-blk) (block-length dst-blk) )))
       ((< max-idx min-idx)
        (checksum ftable))    
    (cond ((plusp diff)
           (princ "splitting")
           (setf src-blk (split-fileblock src-blk (- (block-length src-blk) diff))
                 max-idx (+ max-idx 1)
                 (gethash max-idx ftable) src-blk))
          ((minusp diff)
           ;split the free space
           ))
    (swap-fileblocks ftable max-idx min-idx))) 

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
