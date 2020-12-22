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

(defvar *wins-from*)

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

(defun recursive-combat (depth player-1 winnings-1 player-2 winnings-2)
  (cond
    ((gethash (to-key player-1 winnings-1 player-2 winnings-2) *seen-previously*)
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
     (recursive-combat depth (reverse winnings-1) nil player-2 winnings-2))
    ((and (null player-2) (not (null winnings-2)))
     (recursive-combat depth player-1 winnings-1 (reverse winnings-2) nil))
    (t (progn
         (setf (gethash (to-key player-1 winnings-1 player-2 winnings-2) *seen-previously*) t)
         (bind ((a (pop player-1))
                (b (pop player-2)))
           (if (and (>= (+ (length player-1) (length winnings-1)) a)
                    (>= (+ (length player-2) (length winnings-2)) b))
               (bind ((sub-a (subseq (append player-1 (reverse winnings-1)) 0 a))
                      (sub-b (subseq (append player-2 (reverse winnings-2)) 0 b)))
                 (match (or (gethash (cons sub-a sub-b) *wins-from*)
                            (setf (gethash (cons sub-a sub-b) *wins-from*)
                                  (bind ((*seen-previously* (make-hash-table :test #'equal)))
                                    (recursive-combat (1+ depth) sub-a nil sub-b nil))))
                   ('player-1 (recursive-combat depth
                                                (append player-1)
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

(defun part-2 ()
  (match (->> (file-string 22)
           (split (create-scanner '(:sequence #\Newline #\Newline)))
           (mapcar (lambda (player)
                     (-<> (create-scanner '(:sequence #\Newline))
                       (split <> player)
                       (cdr <>)
                       (mapcar #'read-from-string <>)))))
    ((list player-1 player-2)
     (bind ((*seen-previously* (make-hash-table :test #'equal))
            (*wins-from*       (make-hash-table :test #'equal)))
       (recursive-combat 0 player-1 nil player-2 nil)))))

;; Too high: 32992

