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
  (defpackage :day19
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
  (in-package :day19))

#+nil
(file-lines 19)

#+nil
(file-lines-for "day-19-rules.in")

;; Not used
(defun generate-strings (tree rules)
  (cond
    ((null tree)       (list nil))
    ((characterp tree) (list tree))
    ((and (listp tree)
          (> (length tree) 1)
          (listp (car tree)))
     (apply #'concatenate 'list (mapcar (lambda (x) (generate-strings x rules)) tree)))
    ((numberp tree) (generate-strings (gethash tree rules) rules))
    ((and (listp tree)
          (= 1 (length tree))
          (numberp (car tree)))
     (generate-strings (gethash (car tree) rules) rules))
    (t
     (let ((these (generate-strings (car tree) rules))
           (those (generate-strings (cdr tree) rules)))
       (iter outer
         (for x in these)
         (iter
           (for y in those)
           (if (listp y)
               (if (listp x)
                   (in outer (collecting (append x y)))
                   (in outer (collecting (cons x y))))
               (if (listp x)
                   (in outer (collecting (nconc x (list y))))
                   (in outer (collecting (list x y)))))))))))

;; Not used
(defun expand-regex (tree rules)
  (progn
    (format t "tree: ~a~%" tree)
    (when (and (listp tree)
               (member 'bar tree))
      (format t "(subseq tree 0 (position 'bar tree)): ~a~%" (subseq tree 0 (position 'bar tree)))
      (format t "(subseq tree (1+ (position 'bar tree))): ~a~%" (subseq tree (1+ (position 'bar tree)))))
   (cond
     ((characterp tree) (format nil "~a" tree))
     ((numberp tree)    (expand-regex (gethash tree rules) rules))
     ((member 'bar tree)
      (concatenate 'string
                   "("
                   (expand-regex (subseq tree 0 (position 'bar tree)) rules)
                   "|"
                   (expand-regex (subseq tree (1+ (position 'bar tree))) rules)
                   ")"))
     (t            (->> (mapcar (lambda (x) (expand-regex x rules)) tree)
                     (apply #'concatenate 'string))))))

;; Not used
(defun part-1-alt ()
  (bind ((rules
          (iter
            (with rules = (make-hash-table))
            (for line in (file-lines-for "day-19-rules.in"))
            (match line
              ((ppcre "([0-9]+): \"(.*)\""
                      (read rule-number) (vector char))
               (setf (gethash rule-number rules) char))
              ((ppcre "([0-9]+): (.*)"
                      (read rule-number) spec)
               (let ((numbers (-<> (split "[^-0-9]" spec)
                                (remove "" <> :test #'string-equal)
                                (mapcar #'read-from-string <>))))
                 (push (if (all-matches-as-strings "\\|" spec)
                              (concatenate 'list
                                           (subseq numbers 0 2)
                                           (list 'bar)
                                           (subseq numbers 2))
                              numbers)
                       (gethash rule-number rules)))))
            (finally (return rules))))
         (regex (expand-regex (car (gethash 0 rules)) rules)))
    (format t "regex: ~a~%" regex)
    (->> (file-lines 19)
      (count-if (lambda (line)
                  (member line
                          (all-matches-as-strings regex line)
                          :test #'string-equal))))))

(defun replace-numbers (tree rules)
  (cond
    ((characterp tree) tree)
    ((listp tree)      (if (> (length tree) 1)
                           (cons :sequence
                                 (mapcar (lambda (x) (replace-numbers x rules)) tree))
                           (replace-numbers (car tree) rules)))
    ((numberp tree)    (let ((sub-rules (gethash tree rules)))
                         (if (and (listp sub-rules) (> (length sub-rules) 1))
                             (cons :alternation
                                   (mapcar (lambda (x) (replace-numbers x rules))
                                           (gethash tree rules)))
                             (replace-numbers (if (listp sub-rules)
                                                  (car sub-rules)
                                                  sub-rules)
                                              rules))))))

(defun part-1 ()
  (bind ((rules
          (iter
            (with rules = (make-hash-table))
            (for line in (file-lines-for "day-19-rules.in"))
            (match line
              ((ppcre "([0-9]+): \"(.*)\""
                      (read rule-number) (vector char))
               (setf (gethash rule-number rules) char))
              ((ppcre "([0-9]+): (.*)"
                      (read rule-number) spec)
               (match spec
                 ((ppcre "([0-9]+) ([0-9]+) \\| ([0-9]+) ([0-9]+)"
                         (read rule-1) (read rule-2) (read rule-3) (read rule-4))
                  (progn
                    (push (list rule-3 rule-4) (gethash rule-number rules))
                    (push (list rule-1 rule-2) (gethash rule-number rules))))
                 ((ppcre "([0-9]+) \\| ([0-9]+)"
                         (read rule-1) (read rule-2))
                  (progn
                    (push (list rule-2) (gethash rule-number rules))
                    (push (list rule-1) (gethash rule-number rules))))
                 ((ppcre "([0-9]+) ([0-9]+) ([0-9]+)"
                         (read rule-1) (read rule-2) (read rule-3))
                  (push (list rule-1 rule-2 rule-3) (gethash rule-number rules)))
                 ((ppcre "([0-9]+) ([0-9]+)"
                         (read rule-1) (read rule-2))
                  (push (list rule-1 rule-2) (gethash rule-number rules)))
                 ((ppcre "([0-9]+)"
                         (read rule-1))
                  (push (cons rule-1 nil) (gethash rule-number rules))))))
            (finally (return rules))))
         (tree-0 `(:sequence :start-anchor
                             ,(replace-numbers (car (gethash 0 rules)) rules)
                             :end-anchor))
         (rule-0 (create-scanner tree-0))
         )
    (->> (file-lines 19)
      (count-if (lambda (line) (all-matches rule-0 line))))))

;; Wrong: 355
;; Wrong: 79

;; Not used
(defun replace-numbers-2 (tree rules)
  (cond
    ((characterp tree) tree)
    ((listp tree)      (if (> (length tree) 1)
                           (cons :sequence
                                 (mapcar (lambda (x) (replace-numbers-2 x rules)) tree))
                           (replace-numbers-2 (car tree) rules)))
    ((numberp tree)    (let ((sub-rules (gethash tree rules)))
                         ;; 8 & 11 are never alternations, they're
                         ;; repititions (because that's the input
                         ;; data)
                         (cond
                           ((= tree 8)
                            `(:greedy-repetition 1 nil ,(replace-numbers-2 42 rules)))
                           ((= tree 11)
                            `(:sequence (:greedy-repetition 1
                                                            nil
                                                            ,(replace-numbers-2 42 rules))
                                        (:greedy-repetition 1
                                                            nil
                                                            ,(replace-numbers-2 31 rules))))
                           (t (if (and (listp sub-rules) (> (length sub-rules) 1))
                                  (cons :alternation
                                        (mapcar (lambda (x) (replace-numbers-2 x rules))
                                                (gethash tree rules)))
                                  (replace-numbers-2 (if (listp sub-rules)
                                                         (car sub-rules)
                                                         sub-rules)
                                                     rules))))))))

(defun part-2 ()
  (bind ((rules
          (iter
            (with rules = (make-hash-table))
            (for line in (file-lines-for "day-19-rules.in"))
            (match line
              ((ppcre "([0-9]+): \"(.*)\""
                      (read rule-number) (vector char))
               (setf (gethash rule-number rules) char))
              ((ppcre "([0-9]+): (.*)"
                      (read rule-number) spec)
               (match spec
                 ((ppcre "([0-9]+) ([0-9]+) \\| ([0-9]+) ([0-9]+)"
                         (read rule-1) (read rule-2) (read rule-3) (read rule-4))
                  (progn
                    (push (list rule-3 rule-4) (gethash rule-number rules))
                    (push (list rule-1 rule-2) (gethash rule-number rules))))
                 ((ppcre "([0-9]+) \\| ([0-9]+)"
                         (read rule-1) (read rule-2))
                  (progn
                    (push (list rule-2) (gethash rule-number rules))
                    (push (list rule-1) (gethash rule-number rules))))
                 ((ppcre "([0-9]+) ([0-9]+) ([0-9]+)"
                         (read rule-1) (read rule-2) (read rule-3))
                  (push (list rule-1 rule-2 rule-3) (gethash rule-number rules)))
                 ((ppcre "([0-9]+) ([0-9]+)"
                         (read rule-1) (read rule-2))
                  (push (list rule-1 rule-2) (gethash rule-number rules)))
                 ((ppcre "([0-9]+)"
                         (read rule-1))
                  (push (cons rule-1 nil) (gethash rule-number rules))))))
            (finally (return rules))))
         (tree-42 (replace-numbers 42 rules))
         (tree-31 (replace-numbers 31 rules)))
    (->> (file-lines 19)
      (count-if
       (lambda (line)
         (iter
           (for i from 2 below 10)
           (thereis
            (iter
              (for j from (1- i) downto 1)
              (thereis
               (all-matches
                (create-scanner
                 `(:sequence :start-anchor
                             (:non-greedy-repetition ,i ,i ,tree-42)
                             (:non-greedy-repetition ,j ,j ,tree-31)
                   :end-anchor))
                line))))))))))

;; Wrong: 265
;; Wrong: 313
;; Wrong: 311 (too high)
