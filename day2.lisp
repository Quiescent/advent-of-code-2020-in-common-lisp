(eval-when (:execute :compile-toplevel :load-toplevel)
  (ql:quickload '(:iterate
                  :cl-arrows
                  :metabang-bind
                  :cl-ppcre
                  :trivia
                  :trivia.ppcre))
  (load "./file.lisp")
  (defpackage :day2
    (:use :common-lisp)
    (:use :file)
    (:use :iterate)
    (:use :cl-arrows)
    (:use :trivia)
    (:use :trivia.ppcre)
    (:use :metabang-bind)
    (:use :cl-ppcre))
  (in-package :day2))

#+nil
(file-lines 2)

(defun part-1 ()
  (iter
    (for line in (file-lines 2))
    (match line
      ((ppcre "([0-9]+)-([0-9]+) (\\w): (\\w+)"
              (read i) (read j) (vector letter) target)
       (bind ((letter-count (count-if (lambda (x) (char= x letter)) target)))
         (counting (and (>= letter-count i)
                        (<= letter-count j))))))))

(defun part-2 ()
  (iter
    (for line in (file-lines 2))
    (match line
      ((ppcre "([0-9]+)-([0-9]+) (\\w): (\\w+)"
              (read i) (read j) (vector letter) target)
       (counting (or (and (eq letter (elt target (1- i)))
                          (not (eq letter (elt target (1- j)))))
                     (and (eq letter (elt target (1- j)))
                          (not (eq letter (elt target (1- i)))))))))))
