(eval-when (:execute :compile-toplevel :load-toplevel)
  (ql:quickload '(:iterate
                  :metabang-bind
                  :cl-ppcre
                  :arrow-macros
                  :trivia))
  (load "./file.lisp")
  (defpackage :day1
    (:use :common-lisp)
    (:use :iterate)
    (:use :file)
    (:use :metabang-bind)
    (:use :cl-ppcre)
    (:use :arrow-macros))
  (in-package :day1))

#+nil
(file-lines 1)

#+nil
(line-numbers 1)

(defun part-1 ()
  (iter outer
    (with numbers = (->> (line-numbers 1)
                      (mapcar #'car)))
    (for i from 0)
    (for x in numbers)
    (iter
      (for j from 0)
      (for y in numbers)
      (when (and (/= i j) (= (+ x y) 2020))
        (in outer (leave (* x y)))))))

(defun part-2 ()
  (iter outer
    (with numbers = (->> (line-numbers 1)
                      (mapcar #'car)))
    (for i from 0)
    (for x in numbers)
    (iter
      (for j from 0)
      (for y in numbers)
      (iter
        (for k from 0)
        (for z in numbers)
        (when (and (/= i j k) (= (+ x y z) 2020))
          (in outer (leave (* x y z))))))))
