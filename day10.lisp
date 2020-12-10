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
  (or (gethash current *ways-from*)
      (if (= current max)
          1
          (if (or (> current max)
                  (null xs))
              0
              (iter
                (for next in (remove-if-not (lambda (other) (or (= 1 (- other current))
                                                                (= 2 (- other current))
                                                                (= 3 (- other current))))
                                            xs))
                (for pos = (position next xs))
                (summing (arrangements (concatenate 'list (subseq xs 0 pos) (subseq xs (1+ pos)))
                                       next
                                       max)
                         :into total)
                (finally
                 (return (setf (gethash current *ways-from*) total))))))))

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
