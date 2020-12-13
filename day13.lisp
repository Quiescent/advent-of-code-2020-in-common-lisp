(eval-when (:execute :compile-toplevel :load-toplevel)
  (ql:quickload '(:iterate
                  :cl-ppcre
                  :cl-arrows
                  :trivia
                  :trivia.ppcre
                  :anaphora))
  (load "./file.lisp")
  (defpackage :day13
    (:use :common-lisp)
    (:use :file)
    (:use :cl-ppcre)
    (:use :iterate)
    (:use :cl-arrows)
    (:use :trivia)
    (:use :trivia.ppcre)
    (:use :anaphora))
  (in-package :day13))

#+nil
(file-lines 13)

(defun part-1 ()
  (match (file-lines 13)
    ((list (read depart) buses)
     (let ((buses (->> (split "[^0-9]+" buses)
                    (mapcar #'read-from-string))))
       (iter
         (for bus in buses)
         (for loops = (floor depart bus))
         (for waiting = (- bus (mod depart bus)))
         (finding (* waiting bus) minimizing waiting))))))

;; Wrong: 13

(defun part-2 ()
  (match (file-lines 13)
    ((list _ buses)
     (let ((buses (->> (split "," buses)
                    (mapcar #'read-from-string))))
       (iter
         (for i from 0)
         (for bus in buses)
         (when (eq bus 'X)
           (next-iteration))
         (collecting (cons bus i) :into bus-times)
         (finally
          (return
            (iter
              (with times = 1)
              (with base = (caar bus-times))
              (for bus-time in (cdr bus-times))
              (for (a-inc . _) = bus-time)
              (collecting bus-time into rules at 'end)
              (for multiplier previous a-inc initially 1)
              (collecting multiplier into multipliers)
              (setf times
                    (iter
                      (for i from times below (* times 10000) by (apply #'lcm multipliers))
                      (for current = (* i base))
                      (finding i such-that
                               (iter
                                 (for (bus . diff) in rules)
                                 (always (= 0 (mod (+ current diff) bus)))))))
              (finally (return (* times base)))))))))))

;; Too high: 1683891979003440

;; Used this to discover that the cycle length equals the bus
;; number(!)
(defun rounds-until (a-inc b-inc diff)
  (let ((marked (make-array '(1000000) :initial-element nil)))
    (iter
      (for i from 0 by a-inc below 100000)
      (for cnt from 0)
      (setf (elt marked i) cnt))
    (iter
      (for i from 0 by b-inc below 100000)
      (with start)
      (awhen (and (> (- i diff) 0)
                  (elt marked (- i diff)))
        (format t "it: ~a~%" it)
        (if (null start)
          (setf start it)
          (leave (- it start)))))))
