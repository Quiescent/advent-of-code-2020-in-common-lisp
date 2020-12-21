(eval-when (:execute :load-toplevel :compile-toplevel)
  (ql:quickload '(:iterate
                  :cl-ppcre
                  :cl-arrows
                  :trivia
                  :trivia.ppcre
                  :metabang-bind
                  :anaphora
                  :cl-heap))
  (load "./file.lisp")
  (defpackage :day21
    (:use :common-lisp)
    (:use :file)
    (:use :iterate)
    (:use :cl-ppcre)
    (:use :cl-arrows)
    (:use :trivia)
    (:use :trivia.ppcre)
    (:use :metabang-bind)
    (:use :anaphora)
    (:use :cl-heap))
  (in-package :day21))

#+nil
(file-lines 21)

(defun assign (assignments directory)
  (match directory
    (nil assignments)
    ((list* (list allergen ingredients) rest)
     (iter
       (for xs on ingredients)
       (for ingredient = (car xs))
       (for assigned = (assoc allergen assignments))
       (for ingredient-taken = (find ingredient assignments :key #'cdr))
       (when (or (and (not ingredient-taken)
                      (not assigned))
                 (and (eq (car assigned) allergen)
                      (eq (cdr assigned) ingredient)))
         (thereis (assign (cons (cons allergen ingredient) assignments) rest)))))))

(defun part-1 ()
  (bind (all-instances-ingredients
         (directory (->> (file-lines 21)
                      (mapcar
                       (lambda (line)
                         (match line
                           ((ppcre "([^()]*) \\(contains (.*)\\)"
                                   ingredients allergens)
                            (bind ((is (->> (split " " ingredients)
                                         (mapcar #'read-from-string)))
                                   (as (->> (split ", " allergens)
                                         (mapcar #'read-from-string))))
                              (setf all-instances-ingredients (append all-instances-ingredients is))
                              (iter
                                (for allergen in as)
                                (collecting (list allergen is))))))))
                      (apply #'concatenate 'list)))
         (all-ingredients (->> (mapcar #'cadr directory)
                            (reduce #'union)))
         (safe-ingredients (->> (assign (list) directory)
                             (mapcar #'cdr)
                             (set-difference all-ingredients))))
    (count-if (lambda (ingredient) (member ingredient safe-ingredients))
              all-instances-ingredients)))
