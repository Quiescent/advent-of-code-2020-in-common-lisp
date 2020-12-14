(eval-when (:execute :compile-toplevel :load-toplevel)
  (ql:quickload '(:iterate
                  :cl-ppcre
                  :cl-arrows
                  :trivia
                  :trivia.ppcre
                  :anaphora))
  (load "./file.lisp")
  (defpackage :day14
    (:use :common-lisp)
    (:use :file)
    (:use :iterate)
    (:use :cl-ppcre)
    (:use :cl-arrows)
    (:use :trivia)
    (:use :trivia.ppcre)
    (:use :anaphora))
  (in-package :day14))

#+nil
(file-lines 14)

(defun part-1 ()
  (let ((memory (make-hash-table))
        mask)
    (->> (file-lines 14)
      (map nil
           (lambda (line)
             (match line
               ((ppcre "mask = ([10X]+)"
                       new-mask)
                (setf mask new-mask))
               ((ppcre "mem\\[([0-9]+)\\] = ([0-9]+)"
                       (read address) (read value))
                (setf (gethash address memory)
                      (iter
                        (for x in-string mask)
                        (for i downfrom  (1- (length mask)))
                        (with final-value = value)
                        (case x
                          (#\X nil)
                          (#\1 (setf final-value (logior (ash 1 i)          final-value)))
                          (#\0 (setf final-value (logand (lognot (ash 1 i)) final-value))))
                        (finally
                         (progn

                           (return final-value))))))))))
    (iter
      (for (key value) in-hashtable memory)
      (summing value))))

;; Wrong: 4502173936549

(defun part-2 ()
  (let ((memory (make-hash-table))
        mask)
    (->> (file-lines 14)
      (map nil
           (lambda (line)
             (match line
               ((ppcre "mask = ([10X]+)"
                       new-mask)
                (setf mask new-mask))
               ((ppcre "mem\\[([0-9]+)\\] = ([0-9]+)"
                       (read address) (read value))
                (iter
                  (for x in-string mask)
                  (for i downfrom  (1- (length mask)))
                  (with addresses = (list address))
                  (case x
                    (#\X (setf addresses
                               (iter
                                 (for address in addresses)
                                 (collecting (logand (lognot (ash 1 i)) address))
                                 (collecting (logior (ash 1 i) address)))))
                    (#\1 (map-into addresses
                                   (lambda (address) (logior (ash 1 i) address))
                                   addresses))
                    (#\0 nil))
                  (finally
                   (iter
                     (for address in addresses)
                     (setf (gethash address memory)
                           value)))))))))
    (iter
      (for (key value) in-hashtable memory)
      (summing value))))
