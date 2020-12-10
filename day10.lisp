(eval-when (:execute :load-toplevel :compile-toplevel)
  (ql:quickload '(:iterate
                  :cl-arrows
                  :cl-ppcre
                  :trivia
                  :trivia.ppcre
                  :anaphora))
  (load "./file.lisp")
  (defpackage :day10
    (:use :common-lisp)
    (:use :file)
    (:use :iterate)
    (:use :cl-arrows)
    (:use :cl-ppcre)
    (:use :trivia)
    (:use :trivia.ppcre)
    (:use :anaphora))
  (in-package :day10))

#+nil
(file-lines 10)

(defun part-1 ()
  (iter
    (with remaining = (-<> (line-numbers 10)
                        (mapcar #'car <>)
                        (sort <> #'<)))
    (initially
     (iter
       (for x in remaining)
       (maximizing x)
       (finally (push (+ 3 x) remaining))))
    (with current = 0)
    (for next = (find-if (lambda (other) (or (= 1 (- other current))
                                        (= 3 (- other current))))
                         remaining))
    (for pos = (position next remaining))
    (setf remaining (concatenate 'list (subseq remaining 0 pos) (subseq remaining (1+ pos))))
    (counting (= 1 (- next current)) into ones)
    (counting (= 3 (- next current)) into threes)
    (setf current next)
    (while remaining)
    (finally
     (return (* threes ones)))))

(defvar *ways-from*)

(defun arrangements (xs current max)
  (acond
    ((gethash current *ways-from*)  it)
    ((= current max)                1)
    ((or (> current max) (null xs)) 0)
    (t
     (let ((ways-from
             (->> (remove-if-not (lambda (other) (let ((diff (- other current)))
                                                     (and (>= diff 1)
                                                          (<= diff 3))))
                                 xs)
               (mapcar (lambda (next)
                         (let ((pos (position next xs)))
                           (arrangements (concatenate 'list
                                                      (subseq xs 0 pos)
                                                      (subseq xs (1+ pos)))
                                         next
                                         max))))
               (apply #'+))))
       (setf (gethash current *ways-from*) ways-from)))))

(defun part-2 ()
  (let ((*ways-from* (make-hash-table)))
    (iter
      (with max)
      (with remaining = (-<> (line-numbers 10)
                          (mapcar #'car <>)
                          (sort <> #'<)))
      (initially
       (iter
         (for x in remaining)
         (maximizing x)
         (setf max (+ 3 x))
         (finally (push (+ 3 x) remaining))))
      (leave (arrangements remaining 0 max)))))
