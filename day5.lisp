(eval-when (:execute :compile-toplevel :load-toplevel)
  (ql:quickload '(:iterate
                  :cl-ppcre
                  :metabang-bind
                  :cl-arrows
                  :trivia
                  :trivia.ppcre))
  (load "./file.lisp")
  (load "./hash.lisp")
  (defpackage :day5
    (:use :common-lisp)
    (:use :file)
    (:use :iterate)
    (:use :metabang-bind)
    (:use :cl-ppcre)
    (:use :trivia)
    (:use :trivia.ppcre)
    (:use :cl-arrows)
    (:use :hash))
  (in-package :day5))

#+nil
(file-lines 5)

(defun seat-id (instructions)
  (iter
    (with upper = 127)
    (with lower = 0)
    (with row = nil)
    (for i from 0 below 7)
    (for instruction in-string instructions)
    (match instruction
      (#\F (setf upper (- upper (floor (- (1+ upper) lower) 2))))
      (#\B (setf lower (+ lower (floor (- (1+ upper) lower) 2)))))
    (setf row lower)
    (finally
     (return
       (iter
         (with upper = 7)
         (with lower = 0)
         (for i from 0 below 3)
         (for instruction in-string (subseq instructions 7))
         (match instruction
           (#\L (setf upper (- upper (floor (- (1+ upper) lower) 2))))
           (#\R (setf lower (+ lower (floor (- (1+ upper) lower) 2)))))
         (finally (return (+ (* row 8) lower))))))))

(defun part-1 ()
  (iter
    (for line in (file-lines 5))
    (maximizing (seat-id line))))

(defun part-2 ()
  (iter
    (for line in (file-lines 5))
    (collecting (seat-id line) into result)
    (finally
     (return
       (iter
         (for seat in (sort result #'<))
         (for previous-seat previous seat initially 10000)
         (when (= 2 (abs (- seat previous-seat)))
           (leave (1- seat))))))))

;; wrong: 168
;; wrong: 170
