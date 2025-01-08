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
  "p1 checksum: 60")


(defun parse-input (input-string)
  "turn a string of numeric digits into a map of a file system
returns a list of two entries: first is the list of files, each a list representing (sequence file-id length)
the second is a similar list for the free spaces, each entry in the form (sequence nil length (list of moved files, initially empty)"
  (loop for ch across input-string
        for size = (digit-char-p ch)
        for idx from 0
        until (null ch)
        when (and (evenp idx)
                  (plusp size)
                  )
          collect (list idx (floor idx 2) size) into used
        when (and (oddp idx)
                  (plusp size)
                  )
          collect (list idx nil size nil) into free
        finally (return (list used free))))

(defun not-full (itm)
  (or (and (null (fourth itm))
          ; (plusp (third itm))
           )
      (> (third itm) (used-space itm))))

(defun used-space (free-block)
  (reduce #'+ (mapcar #'a:lastcar (fourth free-block)))) 

(defun ft-flatten (ft)
  "given a processed filetable sorted by the sequence entry, promote the moved file entries to the top level, maintining their order."
  (loop for (seq id size moved) in ft
        unless (null id)
          collect (list seq id size)
        else when (null moved)
               collect (list seq 0 size)
        else
          append (loop for (orig-seq id size) in moved
                       collect (list seq id size))))

(defun split (fb needed-size)
  "split a file block into two parts, one of the requested size and one with the remainder"
  (destructuring-bind (pos id size) fb
    (assert (<= needed-size size) (needed-size)
            "requested size ~a is too large for file block with size ~a" needed-size size)
    `((,pos ,id ,needed-size) (,pos ,id ,(- size needed-size)))))

(defun checksum (raw-ft &key (debug nil))
  "flatten the filetable and then calculate pos*id for each position"
  (loop for (seq id len) integer in (ft-flatten raw-ft)
        with prevlen = 0
        for idx = 0 then (+ idx prevlen)
        when debug
          do (format t "~&~a: idx:~a " (list seq id len) idx)
        sum (loop for i from idx
                  repeat len
                  sum (* id i)
                  when debug do (format t "~a * ~a = ~a, " id i (* id i)))
        do (setf prevlen len)))

(defun checksum2 (raw-ft)
  (let* ((ft (ft-flatten raw-ft))
         (number-of-elements (reduce #'+ (mapcar #'third ft)))
         (positions (a:iota number-of-elements :start 0))
         (file-ids (mapcan (lambda (fb)
                             (make-list (third fb) :initial-element (second fb)))
                           ft)))
    (reduce #'+ (mapcar #'* positions file-ids))))

(defun p1 (fileblocks)
  "move fileblocks from the end of the file table, and put them into the free spaces at the beginning."
  (do* ((used (reverse (first fileblocks)))
        (stopl (floor (length used) 2))
        (free (copy-list (second fileblocks)))
        (current-file (pop used) (pop used))
        (current-free (first free) (find-if #'not-full free))
        (ft-out (list)))
       ((or (null current-free)
            (< (first current-file) (first current-free)))
                                        ;(break)
        (push current-file used)
        (unless (null current-free) (push current-free ft-out))
        (sort (append ft-out used) #'< :key #'car))
    (let ((file-size (third current-file))
          (free-size (- (third current-free) (used-space current-free)))) ;;need to subtract any moved in here
      (when (< free-size file-size)
        (destructuring-bind (file remainder)
            (split current-file free-size) ;;split file
          (setf current-file file)
          (push remainder used))) 
          
      (a:appendf (fourth current-free) (list current-file))
      (unless (not-full current-free)
        (push current-free ft-out)
        ;;   (a:removef free current-free :test #'equal) ;SHOULD be the first one, so SHOULDNT have to look through the whole list
        (pop free) ;; indeed.
        ))))

(defun find-space (freeblocks file)
  (destructuring-bind (file-index file-id file-size)
      file
    (declare (ignore file-id))
    (let ((max-free-pos (position-if-not (a:curry #'> file-index) freeblocks :key #'first)))
      (find-if (lambda (free)
                 (destructuring-bind (free-index free-id free-size moved )
                     free
                   (declare (ignore free-id moved))
                   (and (< free-index file-index)
                    (<= file-size (- free-size (used-space free))))))
               freeblocks
               :end max-free-pos
               ))))

(defun p2 (fileblocks)
  "move the files from the end to the first free block at the front with enough free space"
  (let ((used (reverse (first fileblocks)))
        (free (copy-list (second fileblocks)))
        (ft-out (list)))
    (labels ((move-block (from-file to-free)
               (a:appendf (fourth to-free) (list from-file))
               (splice-new-free from-file))
             (splice-new-free (pattern)
               (destructuring-bind (ps pid pl)
                   pattern
                 (declare (ignore pid))
                 (let ((new-free (list ps nil pl nil))
                       ;(before (position-if (a:rcurry #'< ps) free :from-end t :key #'first))
                       (after (position-if (a:rcurry #'> ps) free :key #'first)))
                   (nconc (subseq free 0 after ;(1+ before)
                                 )
                         (list new-free)
                         (subseq free after)))))
             (grow-previous (file)
               "alternative to splicing, just grow the preceeding free space by the size of the file that was moved"))
      (loop
        (when (endp used)
          (return-from p2 (sort (append ft-out free) #'< :key #'first)))
        (let* ((current-file (pop used))
               (current-free (find-space free current-file)))
          (if (null current-free) ;need to account for holes left behind when moving.
              (push current-file ft-out)
              (a:appendf (fourth current-free) (list current-file))))))))

(defun run (parts-list data)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (checksum (p1 data))))
      (2 (format t "~&Part 2: ~a" (p2 data))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*)))
    (run parts (parse-input (uiop:read-file-string infile-name)))))

(defun test (&rest parts)
  (run parts (parse-input *test-input*)))
