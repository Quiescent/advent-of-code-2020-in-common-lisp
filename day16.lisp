(eval-when (:execute :load-toplevel :compile-toplevel)
  (ql:quickload '(:iterate
                  :cl-ppcre
                  :cl-arrows
                  :metabang-bind
                  :trivia
                  :trivia.ppcre
                  :anaphora))
  (load "./file.lisp")
  (defpackage :day16
    (:use :common-lisp)
    (:use :file)
    (:use :iterate)
    (:use :cl-ppcre)
    (:use :cl-arrows)
    (:use :metabang-bind)
    (:use :trivia)
    (:use :trivia.ppcre)
    (:use :anaphora))
  (in-package :day16))

#+nil
(file-lines 16)

(defvar *error-rate*)

(defun find-rules (line rules)
  (if (null line)
      t
      (progn
        (when (not (iter
                     (with x = (car line))
                     (for (low-1 high-1 low-2 high-2) in rules)
                     (thereis (or (and (>= x low-1)
                                       (<= x high-1))
                                  (and (>= x low-2)
                                       (<= x high-2))))))
          (incf *error-rate* (car line)))
       (find-rules (cdr line) rules))))

(defun part-1 ()
  (let ((rules (->> (file-lines-for "./day16-rules.in")
                 (mapcar (lambda (line)
                           (match line
                             ((ppcre ".*: ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)"
                                     (read low-1) (read high-1) (read low-2) (read high-2))
                              (list low-1 high-1 low-2 high-2)))))))
        (*error-rate* 0))
    (->> (file-lines 16)
      (mapcar (lambda (line) (->> (split "," line)
                          (mapcar #'read-from-string))))
      (map 'nil (lambda (line) (find-rules line rules))))
    *error-rate*))

;; Wrong: 241
;; Wrong: 190

(defun is-valid (line rules)
  (if (null line)
      t
      (and
        (iter
          (with x = (car line))
          (for (low-1 high-1 low-2 high-2) in rules)
          (thereis (or (and (>= x low-1)
                            (<= x high-1))
                       (and (>= x low-2)
                            (<= x high-2)))))
       (is-valid (cdr line) rules))))

(defun part-2 ()
  (let* ((rules (->> (file-lines-for "./day16-rules.in")
                  (mapcar (lambda (line)
                            (match line
                              ((ppcre ".*: ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)"
                                      (read low-1) (read high-1) (read low-2) (read high-2))
                               (list low-1 high-1 low-2 high-2)))))))
         (valid-tickets (->> (file-lines 16)
                          (mapcar (lambda (line) (->> (split "," line)
                                                   (mapcar #'read-from-string))))
                          (remove-if-not (lambda (line) (is-valid line rules)))))
         (possible-labels (iter
                            (for i from 0 below 20)
                            (collecting (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
                                        :result-type 'vector))))
    (iter
      (for ticket in valid-tickets)
      (iter
        (for value in ticket)
        (for ticket-pos from 0)
        (iter
          (for (low-1 high-1 low-2 high-2) in rules)
          (for rule-idx from 0)
          (when (not (or (and (>= value low-1)
                              (<= value high-1))
                         (and (>= value low-2)
                              (<= value high-2))))
            (setf (elt possible-labels ticket-pos)
                  (remove rule-idx (elt possible-labels ticket-pos)))))))
    (let ((positions (iter
                       (with result = (vector 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
                       (for i from 0 below 20)
                       (for single = (position 1 possible-labels :key #'length))
                       (for to-remove = (car (elt possible-labels single)))
                       (setf (elt result single) to-remove)
                       (map-into possible-labels
                                 (lambda (other-set) (remove to-remove other-set))
                                 possible-labels)
                       (finally (return result))))
          (my-ticket (->> (file-lines-for "./day16-ticket.in")
                       (car)
                       (split ",")
                       (map 'vector #'read-from-string))))
      (iter
        (for i from 0 below 20)
        (for position in-vector positions)
        (when (< position 6)
            (multiplying (elt my-ticket i)))))))

;; Wrong: 3800651307113
;; Wrong: 1901546127361

;; 14: 61
;; 19: 53
;; 6: 199
;; 16: 157
;; 5: 197
;; 2: 191
;; 1
;; 4
;; 3
;; 0
;; 8
;; 13
;; 17
;; 15
;; 10
;; 18
;; 7
;; 9
;; 11
;; 12
