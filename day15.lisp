(eval-when (:execute :compile-toplevel :load-toplevel)
  (ql:quickload '(:iterate
                  :cl-ppcre
                  :cl-arrows
                  :trivia
                  :trivia.ppcre
                  :anaphora
                  :metabang-bind))
  (load "./file.lisp")
  (defpackage :day15
    (:use :common-lisp)
    (:use :file)
    (:use :iterate)
    (:use :cl-ppcre)
    (:use :cl-arrows)
    (:use :trivia)
    (:use :trivia.ppcre)
    (:use :anaphora)
    (:use :metabang-bind))
  (in-package :day15))

#+nil
(file-lines 15)

(defun part-1 ()
  (let ((last-spoken (make-hash-table)))
    (iter
      (with last-last)
      (initially
       (iter
         (for number in '(8 0 17 4 1))
         (for i from 1)
         (setf (gethash number last-spoken) i)))
      (for i from 6 to 2020)
      (for last initially 12
           then (prog1
                  (aif (gethash last last-spoken) (- i it) 0)
                  (setf (gethash last last-spoken) i)))
      (setf last-last last)
      (finally (return last-last)))))

(defun part-2 ()
  (let ((last-spoken (make-hash-table)))
    (iter
      (with last-last)
      (initially
       (iter
         (for number in '(8 0 17 4 1))
         (for i from 1)
         (setf (gethash number last-spoken) i)))
      (for i from 6 to 30000000)
      (for last initially 12
           then (prog1
                    (aif (gethash last last-spoken) (- i it) 0)
                  (setf (gethash last last-spoken) i)))
      (setf last-last last)
      (finally (return last-last)))))
