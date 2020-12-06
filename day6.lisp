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
  (iter
    (with group = (make-hash-table :test #'equal))
    (for line in (file-lines 6))
    (when (string-equal line "")
      (summing (iter
                 (for (key value) in-hashtable group)
                 (counting t))
               :into result)
      (setf group (make-hash-table :test #'equal)))
    (iter
      (for char in-string line)
      (setf (gethash char group) t))
    (finally
     (return
       (+ result
          (iter
            (for (key value) in-hashtable group)
            (counting t)))))))

(defun part-2 ()
  (iter
    (with group = (list))
    (for line in (file-lines 6))
    (when (string-equal line "")
      (summing (-> (reduce #'intersection group)
                 (length))
               :into result)
      (setf group (list))
      (next-iteration))
    (push (map 'list #'identity line) group)
    (finally
     (return
       (+ result
          (-> (reduce #'intersection group)
            (length)))))))
