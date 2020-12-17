(eval-when (:execute :load-toplevel :compile-toplevel)
  (ql:quickload '(:iterate
                  :cl-ppcre
                  :cl-arrows
                  :anaphora
                  :metabang-bind
                  :trivia
                  :trivia.ppcre))
  (load "./file.lisp")
  (defpackage :day17
    (:use :common-lisp)
    (:use :file)
    (:use :cl-ppcre)
    (:use :iterate)
    (:use :cl-arrows)
    (:use :anaphora)
    (:use :metabang-bind)
    (:use :trivia)
    (:use :trivia.ppcre))
  (in-package :day17))

#+nil
(file-lines 17)

(defun slice-from (previous current next x-low x-high y-low y-high)
  (iter
    (with new-layer = (make-hash-table :test #'equal))
    (for x from x-low below x-high)
    (iter
      (for y from y-low below y-high)
      (for neighbours =
           (+ (or (and previous (gethash (cons (1- x) (1- y)) previous) 1) 0)
              (or (and previous (gethash (cons x      (1- y)) previous) 1) 0)
              (or (and previous (gethash (cons x      y)      previous) 1) 0)
              (or (and previous (gethash (cons x      (1+ y)) previous) 1) 0)
              (or (and previous (gethash (cons (1+ x) (1+ y)) previous) 1) 0)
              (or (and previous (gethash (cons (1+ x) y)      previous) 1) 0)
              (or (and previous (gethash (cons (1- x) y)      previous) 1) 0)
              (or (and previous (gethash (cons (1- x) (1+ y)) previous) 1) 0)
              (or (and previous (gethash (cons (1+ x) (1- y)) previous) 1) 0)

              (or (and current (gethash (cons (1- x) (1- y)) current) 1) 0)
              (or (and current (gethash (cons x      (1- y)) current) 1) 0)
              (or (and current (gethash (cons x      (1+ y)) current) 1) 0)
              (or (and current (gethash (cons (1+ x) (1+ y)) current) 1) 0)
              (or (and current (gethash (cons (1+ x) y)      current) 1) 0)
              (or (and current (gethash (cons (1- x) y)      current) 1) 0)
              (or (and current (gethash (cons (1- x) (1+ y)) current) 1) 0)
              (or (and current (gethash (cons (1+ x) (1- y)) current) 1) 0)
              
              (or (and next (gethash (cons (1- x) (1- y)) next) 1) 0)
              (or (and next (gethash (cons x      (1- y)) next) 1) 0)
              (or (and next (gethash (cons x      y)      next) 1) 0)
              (or (and next (gethash (cons x      (1+ y)) next) 1) 0)
              (or (and next (gethash (cons (1+ x) (1+ y)) next) 1) 0)
              (or (and next (gethash (cons (1+ x) y)      next) 1) 0)
              (or (and next (gethash (cons (1- x) y)      next) 1) 0)
              (or (and next (gethash (cons (1- x) (1+ y)) next) 1) 0)
              (or (and next (gethash (cons (1+ x) (1- y)) next) 1) 0)))
      (if (and (gethash (cons x y) current)
               (or (= 2 neighbours)
                   (= 3 neighbours)))
          (setf (gethash (cons x y) new-layer) t)
          (when (= 3 neighbours)
            (setf (gethash (cons x y) new-layer) t))))
    (finally (return new-layer))))

(defun part-1 ()
  (iter
    (for i from 0 below 6)
    (with first-layer =
          (iter
            (with layer = (make-hash-table :test #'equal))
            (for line in (file-lines 17))
            (for y from 0)
            (iter
              (for x from 0)
              (for char in-string line)
              (when (char= char #\#)
                (setf (gethash (cons x y) layer) t)))
            (finally (return layer))))
    (with x-low = 0)
    (with x-high = (->> (file-lines 17)
                    (car)
                    (length)))
    (with y-low = 0)
    (with y-high = (->> (file-lines 17)
                     (length)))
    (decf x-low)
    (incf x-high)
    (decf y-low)
    (incf y-high)
    (with space = (list (make-hash-table :test #'equal) first-layer (make-hash-table :test #'equal)))
    (setf space (nconc (cons (make-hash-table :test #'equal)
                             (iter
                               (for tail on space)
                               (for slice = (car tail))
                               (for n-slice = (cadr tail))
                               (for p-slice previous slice)
                               (collecting
                                (slice-from p-slice slice n-slice x-low x-high y-low y-high))))
                       (list (make-hash-table :test #'equal))))
    (finally
     (return
       (iter
         (for table in space)
         (summing
          (iter
            (for (key value) in-hashtable table)
            (counting t))))))))

(defun slice-from-extra-dim (current x-low x-high y-low y-high z-low z-high a-low a-high)
  (iter
    (with new-layer = (make-hash-table :test #'equal))
    (for x from x-low below x-high)
    (iter
      (for y from y-low below y-high)
      (iter
        (for z from z-low below z-high)
        (iter
          (for a from a-low below a-high)
          (for neighbours =
               (iter
                 (for x-delta from -1 below 2)
                 (summing
                  (iter
                    (for y-delta from -1 below 2)
                    (summing
                     (iter
                       (for z-delta from -1 below 2)
                       (summing
                        (iter
                          (for a-delta from -1 below 2)
                          (when (= 0 x-delta y-delta z-delta a-delta)
                            (next-iteration))
                          (summing
                           (or (and (gethash (list (+ x x-delta)
                                                   (+ y y-delta)
                                                   (+ z z-delta)
                                                   (+ a a-delta))
                                             current)
                                    1)
                               0))))))))))
          (if (and (gethash (list x y z a) current)
                   (or (= 2 neighbours)
                       (= 3 neighbours)))
              (setf (gethash (list x y z a) new-layer) t)
              (when (= 3 neighbours)
                (setf (gethash (list x y z a) new-layer) t))))))
    (finally (return new-layer))))

(defun part-2 ()
  (iter
    (for i from 0 below 6)

    (with space =
          (iter
            (with layer = (make-hash-table :test #'equal))
            (for line in (file-lines 17))
            (for y from 0)
            (iter
              (for x from 0)
              (for char in-string line)
              (when (char= char #\#)
                (setf (gethash (list x y 0 0) layer) t)))
            (finally (return layer))))

    (with x-low = 0)
    (with x-high = (->> (file-lines 17)
                     (car)
                     (length)))
    (with y-low = 0)
    (with y-high = (->> (file-lines 17)
                     (length)))
    (with z-low = 0)
    (with z-high = 1)
    (with a-low = 0)
    (with a-high = 1)

    (decf x-low)
    (incf x-high)
    (decf y-low)
    (incf y-high)
    (decf z-low)
    (incf z-high)
    (decf a-low)
    (incf a-high)
    
    (setf space (slice-from-extra-dim space x-low x-high y-low y-high z-low z-high a-low a-high))

    (finally
     (return
       (iter
         (for (key value) in-hashtable space)
         (counting t))))))

;; Wrong: 49
