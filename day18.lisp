(eval-when (:execute :compile-toplevel :load-toplevel)
  (ql:quickload '(:iterate
                  :cl-ppcre
                  :cl-arrows
                  :metabang-bind
                  :anaphora
                  :cl-heap
                  :trivia
                  :trivia.ppcre))
  (load "./file.lisp")
  (defpackage :day18
    (:use :common-lisp)
    (:use :file)
    (:use :iterate)
    (:use :cl-ppcre)
    (:use :cl-arrows)
    (:use :metabang-bind)
    (:use :anaphora)
    (:use :cl-heap)
    (:use :trivia)
    (:use :trivia.ppcre))
  (in-package :day18))

#+nil
(file-lines 18)

(defun evaluate (line)
  (cond
    ((null line)         0)
    ((not (listp line))  line)
    ((listp (car line))  (if (> (length line) 1)
                             (evaluate (cons (funcall (cadr line)
                                                      (evaluate (car line))
                                                      (evaluate (caddr line)))
                                             (cdddr line)))
                             (evaluate (car line))))
    ((= 1 (length line)) (car line))
    (t                   (evaluate (cons (funcall (cadr line)
                                                  (car line)
                                                  (evaluate (caddr line)))
                                         (cdddr line))))))

(defun part-1 ()
  (->> (file-lines 18)
    (mapcar (lambda (line) (->> (concatenate 'string "(" line ")")
                        (read-from-string)
                        (evaluate))))
    (apply #'+)))

;; Wrong: 7894308380093
;; Wrong: 8791458370376
;; Wrong: 5012425023030

(defun group-addition (line)
  (cond
    ((null line)         nil)
    ((not (listp line))  line)
    ((member (car line) '(+ *)) (cons (car line) (group-addition (cdr line))))
    ((and (listp (car line))
          (= 1 (length line)))  (cons (list (group-addition (car line)))
                                      (group-addition (cdr line))))
    ((< (length line) 3) line)
    (t                   (if (eq '+ (cadr line))
                             (progn
                               (iter
                                 (with last-tail)
                                 (for tail on line by #'cddr)
                                 (setf last-tail tail)
                                 (while (eq '+ (cadr tail)))
                                 (collecting (car tail) into result)
                                 (collecting (cadr tail) into result)
                                 (finally
                                  (return (cons (mapcar #'group-addition (nconc result (list (car last-tail))))
                                                (group-addition (cdr last-tail)))))))
                             (cons (car line)
                                   (cons (cadr line)
                                         (group-addition (cddr line))))))))

(defun evaluate-2 (line)
  (cond
    ((null line) 0)
    ((not (listp line)) line)
    (t (iter
         (for symbol in line)
         (with expression-stack = nil)
         (case symbol
           (* (progn
                (when (> (length expression-stack) 2)
                  (iter
                    (while (eq '+ (cadr expression-stack)))
                    (for arg-2 = (pop expression-stack))
                    (for f     = (pop expression-stack))
                    (for arg-1 = (pop expression-stack))
                    (push (funcall f arg-1 arg-2) expression-stack)))
                (push '* expression-stack)))
           (+ (push '+ expression-stack))
           (t (push (evaluate-2 symbol) expression-stack)))
         (finally
          (return
            (iter
              (while (> (length expression-stack) 1))
              (let ((arg-2 (pop expression-stack))
                    (f     (pop expression-stack))
                    (arg-1 (pop expression-stack)))
                (push (funcall f arg-1 arg-2) expression-stack))
              (finally (return (car expression-stack))))))))))

(defun part-2 ()
  (->> (file-lines 18)
    (mapcar (lambda (line) (->> (concatenate 'string "(" line ")")
                        (read-from-string)
                        (evaluate-2))))
    (apply #'+)))

;; Wrong: 29842892620603
;; Wrong: 87318499531987
;; Wrong: 6662412516897
;; Wrong: 31985509407277

