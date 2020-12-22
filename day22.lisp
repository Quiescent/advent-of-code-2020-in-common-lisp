(eval-when (:execute :load-toplevel :compile-toplevel)
  (ql:quickload '(:iterate
                  :cl-arrows
                  :cl-ppcre
                  :trivia
                  :trivia.ppcre
                  :metabang-bind
                  :anaphora
                  :cl-heap))
  (load "./file.lisp")
  (defpackage :day22
    (:use :common-lisp)
    (:use :file)
    (:use :iterate)
    (:use :cl-arrows)
    (:use :cl-ppcre)
    (:use :trivia)
    (:use :trivia.ppcre)
    (:use :metabang-bind)
    (:use :anaphora)
    (:use :cl-heap))
  (in-package :day22))

#+nil
(file-lines 22)

(defun part-1 ()
  (match (->> (file-string 22)
           (split (create-scanner '(:sequence #\Newline #\Newline)))
           (mapcar (lambda (player)
                     (-<> (create-scanner '(:sequence #\Newline))
                       (split <> player)
                       (cdr <>)
                       (mapcar #'read-from-string <>)))))
    ((list player-1 player-2)
     (iter
       (while (and (> (length player-1) 0) (> (length player-2) 0)))
       (for a = (pop player-1))
       (for b = (pop player-2))
       (if (> a b)
           (setf player-1 (nconc player-1 (list a b)))
           (setf player-2 (nconc player-2 (list b a))))
       (finally
        (return
          (iter
            (for i from 1)
            (for x in (reverse (if (null player-1) player-2 player-1)))
            (summing (* i x)))))))))

(defun score (player)
  (iter
    (for i from 1)
    (for x in (reverse player))
    (summing (* i x))))

(defvar *seen-previously*)

(defun to-key (player-1 winnings-1 player-2 winnings-2)
  (bind ((result 0)
         (power 0))
    (iter
      (for x in player-1)
      (incf result (* x (expt 10 power)))
      (incf power 3))
    (iter
      (initially (setf power (+ power (* 3 (1- (length winnings-1))))))
      (for x in winnings-1)
      (incf result (* x (expt 10 power)))
      (decf power 3))
    (setf power (+ power (* 3 (1+ (length winnings-1)))))
    (iter
      (for x in player-2)
      (incf result (* x (expt 10 power)))
      (incf power 3))
    (iter
      (initially (setf power (+ power (* 3 (1- (length winnings-2))))))
      (for x in winnings-2)
      (incf result (* x (expt 10 power)))
      (decf power 3))
    result))

;; Bug in here somewhere :/
(defun recursive-combat (depth player-1 winnings-1 player-2 winnings-2)
  (cond
    ((gethash (list player-1 winnings-1 player-2 winnings-2) *seen-previously*)
     (if (= 0 depth)
         (score player-1)
         'player-1))
    ((or (and (null player-1) (null winnings-1))
         (and (null player-2) (null winnings-2)))
     (if (= 0 depth)
         (progn
           (format t "(append player-2 (reverse winnings-2)): ~a~%" (append player-2 (reverse winnings-2)))
           (format t "(append player-1 (reverse winnings-1)): ~a~%" (append player-1 (reverse winnings-1)))
           (score (if (null player-1)
                     (append player-2 (reverse winnings-2))
                     (append player-1 (reverse winnings-1)))))
         (if (null player-1) 'player-2 'player-1)))
    ((and (null player-1) (not (null winnings-1)))
     (recursive-combat depth
                       (reverse winnings-1)
                       nil
                       player-2
                       winnings-2))
    ((and (null player-2) (not (null winnings-2)))
     (recursive-combat depth
                       player-1
                       winnings-1
                       (reverse winnings-2)
                       nil))
    (t (progn
         (setf (gethash (list player-1 winnings-1 player-2 winnings-2) *seen-previously*) t)
         (bind ((a (pop player-1))
                (b (pop player-2)))
           (if (and (>= (+ (length player-1) (length winnings-1)) a)
                    (>= (+ (length player-2) (length winnings-2)) b))
               (bind ((sub-a (subseq (append player-1 (reverse winnings-1)) 0 a))
                      (sub-b (subseq (append player-2 (reverse winnings-2)) 0 b)))
                 (match (bind ((*seen-previously* (make-hash-table :test #'equal)))
                          (recursive-combat (1+ depth) sub-a nil sub-b nil))
                   ('player-1 (recursive-combat depth
                                                player-1
                                                (cons b (cons a winnings-1))
                                                player-2
                                                winnings-2))
                   ('player-2 (recursive-combat depth
                                                player-1
                                                winnings-1
                                                player-2
                                                (cons a (cons b winnings-2))))))
               (if (> a b)
                   (recursive-combat depth
                                     player-1
                                     (cons b (cons a winnings-1))
                                     player-2
                                     winnings-2)
                   (recursive-combat depth
                                     player-1
                                     winnings-1
                                     player-2
                                     (cons a (cons b winnings-2))))))))))

(defun recursive-combat-alt (depth player-1 player-2 seen)
  (iter
    (when (gethash (list player-1 player-2) seen)
      (if (> depth 0)
          (leave 'player-1)
          (leave (score player-1))))
    (when (null player-1)
      (if (> depth 0)
          (leave 'player-2)
          (leave (score player-2))))
    (when (null player-2)
      (if (> depth 0)
          (leave 'player-1)
          (leave (score player-1))))
    (setf (gethash (list player-1 player-2) seen) t)
    (while (and player-1 player-2))
    (for a = (car player-1))
    (for b = (car player-2))
    (if (and (> (length player-1) a)
             (> (length player-2) b))
        (match (recursive-combat-alt (1+ depth)
                                     (subseq (cdr player-1) 0 a)
                                     (subseq (cdr player-2) 0 b)
                                     (make-hash-table :test #'equal))
          ('player-1 (progn (setf player-1 (append (cdr player-1) (list a b)))
                            (setf player-2 (cdr player-2))))
          ('player-2 (progn (setf player-2 (append (cdr player-2) (list b a)))
                            (setf player-1 (cdr player-1)))))
        (if (> a b)
            (progn (setf player-1 (append (cdr player-1) (list a b)))
                   (setf player-2 (cdr player-2)))
            (progn (setf player-2 (append (cdr player-2) (list b a)))
                   (setf player-1 (cdr player-1)))))))

(defun part-2 ()
  (match (->> (file-string 22)
           (split (create-scanner '(:sequence #\Newline #\Newline)))
           (mapcar (lambda (player)
                     (-<> (create-scanner '(:sequence #\Newline))
                       (split <> player)
                       (cdr <>)
                       (mapcar #'read-from-string <>)))))
    ((list player-1 player-2)
     (recursive-combat-alt 0 player-1 player-2 (make-hash-table :test #'equal)))))

;; Too high: 33600
;; Too high: 32992
;; Too low: 31945
;; 32054
