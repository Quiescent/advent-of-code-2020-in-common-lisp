(eval-when (:execute :load-toplevel :compile-toplevel)
  (ql:quickload '(:iterate
                  :metabang-bind
                  :cl-arrows
                  :cl-ppcre
                  :trivia
                  :trivia.ppcre))
  (load "./file.lisp")
  (defpackage :day6
    (:use :common-lisp)
    (:use :file)
    (:use :metabang-bind)
    (:use :cl-ppcre)
    (:use :cl-arrows)
    (:use :trivia)
    (:use :iterate)
    (:use :trivia.ppcre))
  (in-package :day6))

#+nil
(file-lines 6)

(defun part-1 ()
  (->> (file-string 6)
    (split "\\\n\\\n")
    (remove nil)
    (mapcar (lambda (group)
              (->> (split #\Newline group)
                (mapcar (lambda (line) (map 'list #'identity line)))
                (reduce #'nunion)
                (length))))
    (apply #'+)))

#+nil
(file-string 6)

(defun part-2 ()
  (->> (file-string 6)
    (split "\\\n\\\n")
    (remove nil)
    (mapcar (lambda (group)
              (->> (split #\Newline group)
                (mapcar (lambda (line) (map 'list #'identity line)))
                (reduce #'intersection)
                (length))))
    (apply #'+)))
