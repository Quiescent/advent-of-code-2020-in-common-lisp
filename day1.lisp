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
    (for i from 0)
    (for x in (->> (line-numbers 1)
                (mapcar #'car)))
    (iter
      (for j from 0)
      (for y in (->> (line-numbers 1)
                  (mapcar #'car)))
      (when (and (/= i j) (= (+ x y) 2020))
        (in outer (leave (* x y)))))))

(defun part-2 ()
  (iter outer
    (for i from 0)
    (for x in (->> (line-numbers 1)
                (mapcar #'car)))
    (iter
      (for j from 0)
      (for y in (->> (line-numbers 1)
                  (mapcar #'car)))
      (iter
        (for k from 0)
        (for z in (->> (line-numbers 1)
                    (mapcar #'car)))
        (when (and (/= i j k) (= (+ x y z) 2020))
          (in outer (leave (* x y z))))))))
