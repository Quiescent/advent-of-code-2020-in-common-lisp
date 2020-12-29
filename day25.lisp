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

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun part-1 ()
  (bind ((pub-key-1   15628416)
         (loop-size-1 0)
         (pub-key-2   11161639))
    (declare (type (unsigned-byte 64)
                   pub-key-1
                   pub-key-2
                   loop-size-1))
    (iter
      (for x from 2 below 1000000)
      (with value = (the (unsigned-byte 64) 7))
      (declare (type (unsigned-byte 64) x value))
      (setf value (mod (* value 7)
                       (the (unsigned-byte 64) 20201227)))
      (when (= value pub-key-1)
        (setf loop-size-1 x)
        (finish)))
    (iter
      (for x from 1 below loop-size-1)
      (with value = pub-key-2)
      (declare (type (unsigned-byte 64) x value))
      (setf value (mod (* value pub-key-2)
                       (the (unsigned-byte 64) 20201227)))
      (finally (return value)))))

;; Wrong: 10163600
