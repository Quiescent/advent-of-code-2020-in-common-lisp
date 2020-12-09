(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload '(:iterate
                  :cl-ppcre
                  :cl-arrows
                  :metabang-bind
                  :trivia
                  :trivia.ppcre))
  (load "./file.lisp")
  (defpackage :day9
    (:use :common-lisp)
    (:use :file)
    (:use :cl-ppcre)
    (:use :iterate)
    (:use :cl-arrows)
    (:use :metabang-bind)
    (:use :trivia)
    (:use :trivia.ppcre))
  (in-package :day9))

#+nil
(file-lines 9)

(defun not-sum (number numbers)
  (iter outer
    (for x in-vector numbers)
    (for i from 0)
    (iter
      (for y in-vector numbers)
      (for j from 0)
      (when (and (/= i j)
                 (= number (+ x y)))
        (in outer (leave t))))))

(defun part-1 ()
  (iter
    (with numbers = (->> (line-numbers 9)
                     (map 'vector #'car)))
    (for number in-vector numbers)
    (for i from 0)
    (when (< i 25)
      (next-iteration))
    (when (not (not-sum number (subseq numbers (- i 25) i)))
      (leave number))))

(defun part-2 ()
  (iter
    (with numbers = (->> (line-numbers 9)
                      (map 'vector #'car)))
    (with range = '())
    (with range-sum = 0)
    (for number in-vector numbers)
    (when (= range-sum 31161678)
      (leave
       (iter
         (for number in range)
         (minimizing number into min)
         (maximizing number into max)
         (finally (return (+ min max))))))
    (when (< range-sum 31161678)
      (push number range)
      (incf range-sum number))
    (iter
      (while (> range-sum 31161678))
      (bind ((reversed (nreverse range)))
        (decf range-sum (car reversed))
        (setf range (nreverse (cdr reversed)))))))
