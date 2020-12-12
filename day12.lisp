(eval-when (:execute :load-toplevel :compile-toplevel)
  (ql:quickload '(:iterate
                  :cl-ppcre
                  :cl-arrows
                  :trivia
                  :trivia.ppcre
                  :anaphora))
  (load "./file.lisp")
  (defpackage :day12
    (:use :common-lisp)
    (:use :file)
    (:use :cl-ppcre)
    (:use :iterate)
    (:use :cl-arrows)
    (:use :trivia)
    (:use :trivia.ppcre)
    (:use :anaphora))
  (in-package :day12))

#+nil
(file-lines 12)

(defun part-1 ()
  (let ((x   0)
        (y   0)
        (angle 180))
    (->> (file-lines 12)
      (map nil
           (lambda (line)
             (match line
               ((ppcre "E([0-9]+)"
                       (read amt))
                (decf x amt))
               ((ppcre "S([0-9]+)"
                       (read amt))
                (incf y amt))
               ((ppcre "W([0-9]+)"
                       (read amt))
                (incf x amt))
               ((ppcre "N([0-9]+)"
                       (read amt))
                (decf y amt))
               ((ppcre "L([0-9]+)"
                       (read amt))
                (setf angle (mod (- angle amt) 360)))
               ((ppcre "R([0-9]+)"
                       (read amt))
                (setf angle (mod (+ angle amt) 360)))
               ((ppcre "F([0-9]+)"
                       (read amt))
                (case angle
                  (0   (incf x amt))
                  (90  (decf y amt))
                  (180 (decf x amt))
                  (270 (incf y amt))))))))
    (+ (abs x) (abs y))))

;; Wrong: 1859
;; Wrong: 1863

(defun part-2 ()
  (let (tmp
        (x-w -10)
        (y-w -1)
        (x   0)
        (y   0))
    (labels ((r90 ()
               (setf tmp x-w
                     x-w y-w
                     y-w (- 0 tmp)))
             (l90 ()
               (setf tmp x-w
                     x-w (- 0 y-w)
                     y-w tmp)))
     (->> (file-lines 12)
       (map nil
            (lambda (line)
              (match line
                ((ppcre "E([0-9]+)"
                        (read amt))
                 (decf x-w amt))
                ((ppcre "S([0-9]+)"
                        (read amt))
                 (incf y-w amt))
                ((ppcre "W([0-9]+)"
                        (read amt))
                 (incf x-w amt))
                ((ppcre "N([0-9]+)"
                        (read amt))
                 (decf y-w amt))
                ((ppcre "L([0-9]+)"
                        (read amt))
                 (case amt
                   (90  (l90))
                   (180 (progn (l90) (l90)))
                   (270 (progn (l90) (l90) (l90)))))
                ((ppcre "R([0-9]+)"
                        (read amt))
                 (case amt
                   (90  (r90))
                   (180 (progn (r90) (r90)))
                   (270 (progn (r90) (r90) (r90)))))
                ((ppcre "F([0-9]+)"
                        (read amt))
                 (iter
                   (for i from 0 below amt)
                   (incf x x-w)
                   (incf y y-w))))))))
    (+ (abs x) (abs y))))

;; Wrong: 19840
