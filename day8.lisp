(eval-when (:execute :compile-toplevel :load-toplevel)
  (ql:quickload '(:iterate
                  :cl-arrows
                  :cl-ppcre
                  :trivia
                  :trivia.ppcre
                  :metabang-bind))
  (load "./file.lisp")
  (defpackage :day8
    (:use :common-lisp)
    (:use :file)
    (:use :cl-arrows)
    (:use :cl-ppcre)
    (:use :trivia)
    (:use :trivia.ppcre)
    (:use :iterate)
    (:use :metabang-bind))
  (in-package :day8))

#+nil
(file-lines 8)

(defun parse-program ()
  (iter
    (for line in (file-lines 8))
    (match line
      ((ppcre "jmp ([-+0-9]+)"
              (read amount))
       (collecting `(jmp ,amount) :result-type 'vector))
      ((ppcre "acc ([-+0-9]+)"
              (read amount))
       (collecting `(acc ,amount) :result-type 'vector))
      ((ppcre "nop ([-+0-9]+)"
              (read amount))
       (collecting `(nop ,amount) :result-type 'vector)))))

(defun part-1 ()
  (iter
    (with pointer = 0)
    (with program = (parse-program))
    (with acc = 0)
    (adjoining pointer into seen)
    (match (elt program pointer)
      ((list 'jmp amt)
       (incf pointer amt))
      ((list 'acc amt)
       (incf acc amt)
       (incf pointer))
      ((list 'nop amt)
       (incf pointer)))
    (while (not (member pointer seen)))
    (finally (return acc))))

(defun part-2 ()
  (iter outer
    (with program)
    (setf program (parse-program))
    (for i from 0 below (length program))
    (match (elt program i)
      ((list 'jmp amt)
       (setf (elt program i) (list 'nop amt)))
      ((list 'nop amt)
       (setf (elt program i) (list 'jmp amt))))
    (iter
      (with pointer = 0)
      (with acc = 0)
      (adjoining pointer into seen)
      (match (elt program pointer)
        ((list 'jmp amt)
         (incf pointer amt))
        ((list 'acc amt)
         (incf acc amt)
         (incf pointer))
        ((list 'nop _)
         (incf pointer)))
      (when (or (< pointer 0)
                (>= pointer (length program)))
        (in outer (leave acc)))
      (when (member pointer seen)
        (finish)))))

;; Wrong: 336
