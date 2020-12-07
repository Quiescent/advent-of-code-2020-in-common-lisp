(eval-when (:execute :compile-toplevel :load-toplevel)
  (ql:quickload '(:iterate
                  :cl-arrows
                  :cl-ppcre
                  :trivia
                  :trivia.ppcre
                  :metabang-bind))
  (load "./file.lisp")
  (defpackage :day7
    (:use :common-lisp)
    (:use :file)
    (:use :metabang-bind)
    (:use :cl-arrows)
    (:use :iterate)
    (:use :cl-ppcre)
    (:use :trivia)
    (:use :trivia.ppcre))
  (in-package :day7))

#+nil
(file-lines 7)

(defun has-path (dest root graph)
  (or (not (null (assoc dest (gethash root graph) :test #'string-equal)))
      (some (lambda (node) (has-path dest (car node) graph)) (gethash root graph))))

(defun part-1 ()
  (iter
    (with graph = (make-hash-table :test #'equal))
    (for line in (file-lines 7))
    (match line
      ((ppcre "([a-zA-Z ]+) bags contain (.*)\\."
              container contents)
       (->> (split ", " contents)
         (map 'nil (lambda (rule)
                     (match rule
                       ((ppcre "([0-9]+) ([a-zA-Z ]+?) bag"
                               (read count) colour)
                        (push (cons colour count) (gethash container graph)))))))))
    (finally
     (return
       (iter
         (for (key value) in-hashtable graph)
         (when (not (string-equal key "shiny gold"))
           (counting (has-path "shiny gold" key graph))))))))

(defvar *containers* 0)

(defun total-bags (node graph)
  (progn
    (format t "node: ~a~%" node)
    (when (not (null (gethash node graph)))
      (map 'nil (lambda (rule)
                  (iter
                    (for i from 0 below (cdr rule))
                    (incf *containers*)
                    (total-bags (car rule) graph)))
              (gethash node graph)))))

(defun part-2 ()
  (iter
    (with graph = (make-hash-table :test #'equal))
    (for line in (file-lines 7))
    (match line
      ((ppcre "([a-zA-Z ]+) bags contain (.*)\\."
              container contents)
       (->> (split ", " contents)
         (map 'nil (lambda (rule)
                     (match rule
                       ((ppcre "([0-9]+) ([a-zA-Z ]+?) bag"
                               (read count) colour)
                        (push (cons colour count) (gethash container graph)))))))))
    (finally
     (return
       (bind ((*containers* 0))
         (total-bags "shiny gold" graph)
         *containers*)))))

;; Wrong: 49233
