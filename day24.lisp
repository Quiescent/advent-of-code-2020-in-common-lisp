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
  (defpackage :day24
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
  (in-package :day24))

#+nil
(file-lines 24)

(defun to-directions (line)
  (iter
    (with i = 0)
    (while (< i (length line)))
    (for a = (aref line i))
    (for b = (and (< (1+ i) (length line))
                  (aref line (1+ i))))
    (collecting
     (match (list a b)
       ((list #\w _)   'west)
       ((list #\e _)   'east)
       ((list #\n #\e) (progn (incf i) 'north-east))
       ((list #\n #\w) (progn (incf i) 'north-west))
       ((list #\s #\w) (progn (incf i) 'south-west))
       ((list #\s #\e) (progn (incf i) 'south-east))))
    (incf i)))

(defun flip-tile-at (directions grid)
  (iter
    (with x = 0)
    (with y = 0)
    (with z = 0)
    (for direction in directions)
    (match direction
      ('west       (progn (decf x) (decf y)))
      ('east       (progn (incf x) (incf y)))
      ('north-east (progn (incf y) (incf z)))
      ('north-west (progn (decf x) (incf z)))
      ('south-west (progn (decf y) (decf z)))
      ('south-east (progn (incf x) (decf z))))
    (finally (setf (gethash (list x y z) grid)
                   (not (gethash (list x y z) grid))))))

(defun part-1 ()
  (bind ((grid (make-hash-table :test #'equal)))
    (->> (file-lines 24)
      (mapcar #'to-directions)
      (map 'nil
           (lambda (directions)
             (flip-tile-at directions grid))))
    (iter
      (for (key value) in-hashtable grid)
      (counting (eq value t)))))

(defun game-of-art (current-grid new-grid min-x max-x min-y max-y min-z max-z)
  (iter
    (for x from min-x to max-x)
    (iter
      (for y from min-y to max-y)
      (iter
        (for z from min-z to max-z)
        (for w  = (or (and (gethash (list (1- x) (1- y) z)      current-grid) 1) 0))
        (for e  = (or (and (gethash (list (1+ x) (1+ y) z)      current-grid) 1) 0))
        (for ne = (or (and (gethash (list x      (1+ y) (1+ z)) current-grid) 1) 0))
        (for nw = (or (and (gethash (list (1- x) y      (1+ z)) current-grid) 1) 0))
        (for se = (or (and (gethash (list (1+ x) y      (1- z)) current-grid) 1) 0))
        (for sw = (or (and (gethash (list x      (1- y) (1- z)) current-grid) 1) 0))
        (for neighbours = (+ w e ne nw se sw))
        (if (and (gethash (list x y z) current-grid)
                 (not (or (= neighbours 0)
                          (> neighbours 2))))
            (setf (gethash (list x y z) new-grid) t)
            (when (= 2 neighbours)
              (setf (gethash (list x y z) new-grid) t)))))
    (finally (return new-grid))))

(defun part-2 ()
  (bind ((grid (make-hash-table :test #'equal))
         min-x
         max-x
         min-y
         max-y
         min-z
         max-z)
    (->> (file-lines 24)
      (mapcar #'to-directions)
      (map 'nil
           (lambda (directions)
             (flip-tile-at directions grid))))
    (iter
      (for (key value) in-hashtable grid)
      (for (x y z) = key)

      (minimizing x into min-x-found)
      (maximizing x into max-x-found)

      (minimizing y into min-y-found)
      (maximizing y into max-y-found)

      (minimizing z into min-z-found)
      (maximizing z into max-z-found)
      (finally
       (setf min-x (1- min-x-found)
             max-x (1+ max-x-found)
             min-y (1- min-y-found)
             max-y (1+ max-y-found)
             min-z (1- min-z-found)
             max-z (1+ max-z-found))))
    (iter
      (repeat 100)
      (format t "tiles: ~a~%" (iter
                                (for (key value) in-hashtable grid)
                                (counting (eq value t))))
      (setf grid (game-of-art grid
                              (make-hash-table :test #'equal)
                              min-x
                              max-x
                              min-y
                              max-y
                              min-z
                              max-z))
      (iter
      (for (key value) in-hashtable grid)
      (for (x y z) = key)

      (minimizing x into min-x-found)
      (maximizing x into max-x-found)

      (minimizing y into min-y-found)
      (maximizing y into max-y-found)

      (minimizing z into min-z-found)
      (maximizing z into max-z-found)
      (finally
       (setf min-x (1- min-x-found)
             max-x (1+ max-x-found)
             min-y (1- min-y-found)
             max-y (1+ max-y-found)
             min-z (1- min-z-found)
             max-z (1+ max-z-found)))))
    (iter
      (for (key value) in-hashtable grid)
      (counting (eq value t)))))
