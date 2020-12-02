(eval-when (:execute :compile-toplevel :load-toplevel)
  (ql:quickload '(:iterate
                  :arrow-macros
                  :metabang-bind
                  :cl-ppcre
                  :trivia))
  (load "./file.lisp")
  (defpackage :day2
    (:use :common-lisp)
    (:use :file)
    (:use :iterate)
    (:use :arrow-macros)
    (:use :metabang-bind)
    (:use :cl-ppcre))
  (in-package :day2))

#+nil
(file-lines 2)

(defun part-1 ()
  (iter
    (for line in (file-lines 2))
    (for words = (split " " line))
    (for range = (->> (car words)
                   (split "-")
                   (mapcar #'read-from-string)))
    (for letter = (-> (cadr words)
                    (elt 0)))
    (for letter-count = (count-if (lambda (x) (char-equal x letter)) (caddr words)))
    (counting (and (>= letter-count (car range))
                   (<= letter-count (cadr range))))))

(defun part-2 ()
  (iter
    (for line in (file-lines 2))
    (for words = (split " " line))
    (for range = (->> (car words)
                   (split "-")
                   (mapcar #'read-from-string)
                   (mapcar #'1-)))
    (for letter = (-> (cadr words)
                    (elt 0)))
    (counting (or (and (eq letter (elt (caddr words) (car range)))
                       (not (eq letter (elt (caddr words) (cadr range)))))
                  (and (eq letter (elt (caddr words) (cadr range)))
                       (not (eq letter (elt (caddr words) (car range)))))))))
