(eval-when (:execute :compile-toplevel :load-toplevel)
  (ql:quickload '(:iterate
                  :cl-ppcre
                  :cl-arrows
                  :metabang-bind
                  :anaphora
                  :trivia
                  :trivia.ppcre
                  :cl-heap))
  (load "./file.lisp")
  (defpackage :day25
    (:use :common-lisp)
    (:use :file)
    (:use :iterate)
    (:use :cl-ppcre)
    (:use :cl-arrows)
    (:use :metabang-bind)
    (:use :anaphora)
    (:use :trivia)
    (:use :trivia.ppcre)
    (:use :cl-heap))
  (in-package :day25))

#+nil
(file-lines 25)

(defun part-1 ()
  (bind ((pub-key-1 15628416)
         loop-size-1
         (pub-key-2 11161639))
    (iter
      (for x from 2 below 100000000)
      (with value = 7)
      (setf value (mod (* value 7) 20201227))
      (when (= value pub-key-1)
        (setf loop-size-1 x)
        (finish)))
    (iter
      (for x from 1 below loop-size-1)
      (with value = pub-key-2)
      (setf value (mod (* value pub-key-2) 20201227))
      (finally (return value)))))

;; Wrong: 10163600
