(eval-when (:execute :compile-toplevel :load-toplevel)
  (ql:quickload '(:iterate
                  :cl-ppcre
                  :cl-arrows
                  :trivia
                  :trivia.ppcre
                  :anaphora))
  (load "./file.lisp")
  (defpackage :day11
    (:use :common-lisp)
    (:use :file)
    (:use :iterate)
    (:use :cl-ppcre)
    (:use :cl-arrows)
    (:use :trivia)
    (:use :trivia.ppcre)
    (:use :anaphora))
  (in-package :day11))

#+nil
(file-lines 11)

(defun part-1 ()
  (iter
    (with seating = (->> (file-lines 11)
                      (map 'vector #'identity)))
    (with y-dim = (1- (length seating)))
    (with x-dim = (1- (length (elt seating 0))))
    (for next-grid = (iter
                       (for line in-vector seating)
                       (collecting (copy-seq line) :result-type 'vector)))
    (for changed = nil)
    (iter
      (for row in-vector seating)
      (for y from 0)
      (iter
        (for chair in-string row)
        (for x from 0)
        (when (char= chair #\.)
          (setf (elt (elt next-grid y) x) #\.)
          (next-iteration))
        (for u  = (or (and (> y 0)                 (char= #\# (elt (elt seating (1- y)) x))      1) 0))
        (for ur = (or (and (> y 0)     (< x x-dim) (char= #\# (elt (elt seating (1- y)) (1+ x))) 1) 0))
        (for r  = (or (and (< x x-dim)             (char= #\# (elt (elt seating y)      (1+ x))) 1) 0))
        (for dr = (or (and (< y y-dim) (< x x-dim) (char= #\# (elt (elt seating (1+ y)) (1+ x))) 1) 0))
        (for d  = (or (and (< y y-dim)             (char= #\# (elt (elt seating (1+ y)) x))      1) 0))
        (for dl = (or (and (< y y-dim) (> x 0)     (char= #\# (elt (elt seating (1+ y)) (1- x))) 1) 0))
        (for l  = (or (and (> x 0)                 (char= #\# (elt (elt seating y)      (1- x))) 1) 0))
        (for ul = (or (and (> x 0)     (> y 0)     (char= #\# (elt (elt seating (1- y)) (1- x))) 1) 0))
        (for adjacent = (+ u ur r dr d dl l ul))
        (for new-seat =
             (cond
               ((=  adjacent 0) #\#)
               ((>= adjacent 4) #\L)
               (t               chair)))
        (when (not (char= new-seat chair))
          (setf changed t))
        (setf (elt (elt next-grid y) x) new-seat)))
    (while changed)
    (setf seating next-grid)
    (finally
     (return
       (iter
         (for row in-vector seating)
         (summing
          (iter
            (for chair in-string row)
            (counting (char= chair #\#)))))))))

(defun part-2 ()
  (iter
    (with seating = (->> (file-lines 11)
                      (map 'vector #'identity)))
    (with y-dim = (length seating))
    (with x-dim = (length (elt seating 0)))
    (for next-grid = (iter
                       (for line in-vector seating)
                       (collecting (copy-seq line) :result-type 'vector)))
    (for changed = nil)
    (iter
      (for row in-vector seating)
      (for y from 0)
      (iter
        (for chair in-string row)
        (for x from 0)
        (when (char= chair #\.)
          (setf (elt (elt next-grid y) x) #\.)
          (next-iteration))
        (for u  = (or (and (eq #\# (iter
                                     (for y-iter from (1- y) downto 0)
                                     (awhen (member (elt (elt seating y-iter) x) '(#\# #\L))
                                       (leave (car it)))))
                           1)
                      0))
        (for ur = (or (and (eq #\# (iter
                                     (for y-iter from (1- y) downto 0)
                                     (for x-iter from (1+ x) below x-dim)
                                     (awhen (member (elt (elt seating y-iter) x-iter) '(#\# #\L))
                                       (leave (car it)))))
                           1)
                      0))
        (for r  = (or (and (eq #\# (iter
                                     (for x-iter from (1+ x) below x-dim)
                                     (awhen (member (elt (elt seating y) x-iter) '(#\# #\L))
                                       (leave (car it)))))
                           1)
                      0))
        (for dr = (or (and (eq #\# (iter
                                     (for x-iter from (1+ x) below x-dim)
                                     (for y-iter from (1+ y) below y-dim)
                                     (awhen (member (elt (elt seating y-iter) x-iter) '(#\# #\L))
                                       (leave (car it)))))
                           1)
                      0))
        (for d  = (or (and (eq #\# (iter
                                     (for y-iter from (1+ y) below y-dim)
                                     (awhen (member (elt (elt seating y-iter) x) '(#\# #\L))
                                       (leave (car it)))))
                           1)
                      0))
        (for dl = (or (and (eq #\# (iter
                                     (for y-iter from (1+ y) below y-dim)
                                     (for x-iter from (1- x) downto 0)
                                     (awhen (member (elt (elt seating y-iter) x-iter) '(#\# #\L))
                                       (leave (car it)))))
                           1)
                      0))
        (for l  = (or (and (eq #\# (iter
                                     (for x-iter from (1- x) downto 0)
                                     (awhen (member (elt (elt seating y) x-iter) '(#\# #\L))
                                       (leave (car it)))))
                           1)
                      0))
        (for ul = (or (and (eq #\# (iter
                                     (for x-iter from (1- x) downto 0)
                                     (for y-iter from (1- y) downto 0)
                                     (awhen (member (elt (elt seating y-iter) x-iter) '(#\# #\L))
                                       (leave (car it)))))
                           1)
                      0))
        (for adjacent = (+ u ur r dr d dl l ul))
        (for new-seat =
             (cond
               ((=  adjacent 0) #\#)
               ((>= adjacent 5) #\L)
               (t               chair)))
        (when (not (char= new-seat chair))
          (setf changed t))
        (setf (elt (elt next-grid y) x) new-seat)))
    (while changed)
    (for tmp = seating)
    (setf seating next-grid)
    (setf next-grid seating)
    (finally
     (return
       (iter
         (for row in-vector seating)
         (summing
          (iter
            (for chair in-string row)
            (counting (char= chair #\#)))))))))
