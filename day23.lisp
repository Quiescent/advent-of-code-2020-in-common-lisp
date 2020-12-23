(eval-when (:execute :compile-toplevel :load-toplevel)
  (ql:quickload '(:iterate
                  :cl-ppcre
                  :cl-arrows
                  :trivia
                  :trivia.ppcre
                  :metabang-bind
                  :anaphora
                  :cl-heap))
  (load "./file.lisp")
  (defpackage :day23
    (:use :common-lisp)
    (:use :file)
    (:use :iterate)
    (:use :cl-ppcre)
    (:use :cl-arrows)
    (:use :trivia)
    (:use :trivia.ppcre)
    (:use :metabang-bind)
    (:use :anaphora)
    (:use :cl-heap))
  (in-package :day23))

#+nil
(file-lines 23)

(defun make-move (current-cup cups len)
  (bind ((next-1 (gethash current-cup cups))
         (next-2 (gethash next-1      cups))
         (next-3 (gethash next-2      cups))
         (next-4 (gethash next-3      cups))
         (destination (iter
                        (with dest = (1- current-cup))
                        (initially (setf dest (mod dest len)))
                        (while (or (= dest current-cup) (member dest (list next-1 next-2 next-3))))
                        (decf dest)
                        (setf dest (mod dest len))
                        (while (or (= dest current-cup) (member dest (list next-1 next-2 next-3))))
                        (finally (return dest)))))
    (setf (gethash current-cup cups) next-4)
    (setf (gethash next-3 cups)      (gethash destination cups))
    (setf (gethash destination cups) next-1)
    (gethash current-cup cups)))

#+nil
'(5 3 2 6 0 8 1 4 7)

#+nil
'(2 7 8 0 1 4 3 5 6)

(defun part-1 ()
  (bind ((first-ordering (map 'vector #'identity '(5 3 2 6 0 8 1 4 7)))
         (len            (length first-ordering))
         (cups           (make-hash-table)))
    (iter
      (for cup in-vector first-ordering)
      (for i from 0)
      (setf (gethash cup cups) (aref first-ordering (mod (1+ i) len))))
    (iter
      (with current-cup = 5)
      (repeat 100)
      (setf current-cup (make-move current-cup cups len))
      (finally
       (return
         (iter
           (with start = 0)
           (for next = (gethash start cups))
           (while (/= next 0))
           (collecting (-> (format nil "~a" (1+ next)) (aref 0)) :result-type 'string)
           (setf start next)))))))

;; Wrong: 58619437
;; Wrong: 82439657

(defun make-move-2 (current-cup cups len)
  (bind ((next-1 (aref cups current-cup))
         (next-2 (aref cups      next-1))
         (next-3 (aref cups      next-2))
         (next-4 (aref cups      next-3))
         (destination (iter
                        (with dest = (1- current-cup))
                        (initially (setf dest (mod dest len)))
                        (while (or (= dest current-cup) (member dest (list next-1 next-2 next-3))))
                        (decf dest)
                        (setf dest (mod dest len))
                        (while (or (= dest current-cup) (member dest (list next-1 next-2 next-3))))
                        (finally (return dest)))))
    (setf (aref cups current-cup) next-4)
    (setf (aref cups next-3)      (aref cups destination))
    (setf (aref cups destination) next-1)
    (aref cups current-cup)))

(defun part-2 ()
  (bind ((first-ordering (map 'vector #'identity '(5 3 2 6 0 8 1 4 7)))
         (len            1000000)
         (cups           (make-array (list len))))
    (iter
      (for cup in-vector first-ordering)
      (for i from 0 below 8)
      (setf (aref cups cup) (aref first-ordering (mod (1+ i) len))))
    (iter
      (for i from 9 below len)
      (setf (aref cups i) (1+ i)))
    (setf (aref cups 7) 9)
    (setf (aref cups (1- len)) 5)
    (iter
      (with current-cup = 5)
      (repeat 10000000)
      (setf current-cup (make-move-2 current-cup cups len))
      (finally
       (return
         (* (1+ (aref cups 0))
            (1+ (aref cups (aref cups 0)))))))))
