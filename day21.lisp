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

(defun assign (assigned-allergens assigned-ingredients directory)
  (match directory
    (nil assigned-ingredients)
    ((list* (list allergen ingredients) rest)
     (iter
       (for ingredient in ingredients)
       (for assigned-ingredient = (gethash allergen   assigned-allergens))
       (for assigned-allergen   = (gethash ingredient assigned-ingredients))
       (when (or (and (not assigned-ingredient)
                      (not assigned-allergen))
                 (and (eq assigned-allergen allergen)
                      (eq assigned-ingredient ingredient)))
         (setf (gethash allergen   assigned-allergens)   ingredient)
         (setf (gethash ingredient assigned-ingredients) allergen)
         (thereis (assign assigned-allergens assigned-ingredients rest))
         (remhash allergen   assigned-allergens)
         (remhash ingredient assigned-ingredients))))))

(defun part-1 ()
  (bind (all-instances-ingredients
         (directory (make-hash-table)))
    (->> (file-lines 21)
      (map
       'nil
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
                (aif (gethash allergen directory)
                     (setf (gethash allergen directory) (intersection is it))
                     (setf (gethash allergen directory) is)))))))))
    (bind ((safe-ingredients (->> (iter
                                    (for (key value) in-hashtable directory)
                                    (unioning value))
                               (set-difference all-instances-ingredients)
                               (remove-duplicates))))
     (count-if (lambda (ingredient) (member ingredient safe-ingredients))
               all-instances-ingredients))))

(defun part-2 ()
  (bind (all-instances-ingredients
         (directory (make-hash-table)))
    (->> (file-lines 21)
      (map
       'nil
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
                (aif (gethash allergen directory)
                     (setf (gethash allergen directory) (intersection is it))
                     (setf (gethash allergen directory) is)))))))))
    (iter
      (with assignments = (iter
                            (for (key value) in-hashtable directory)
                            (collecting (cons key value))))
      (for next-assignment = (find 2 assignments :key #'length))
      (collecting next-assignment into result)
      (map-into assignments
                (lambda (assignment)
                  (remove (cadr next-assignment) assignment))
                assignments)
      (while (some (lambda (assignment) (>= (length assignment) 2)) assignments))
      (finally
       (return
         (format nil "~{~a~^,~}"
                 (->> (sort result #'string-lessp :key (lambda (item) (-> (car item) (symbol-name))))
                   (mapcar (lambda (item) (->> (cadr item)
                                       (symbol-name)
                                       (string-downcase)))))))))))

;; Wrong: jmvxx,lkv,kfgln,pqqks,pqrvc

