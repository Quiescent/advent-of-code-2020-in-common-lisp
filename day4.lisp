(eval-when (:execute :load-toplevel :compile-toplevel)
  (ql:quickload '(:iterate
                  :cl-arrows
                  :metabang-bind
                  :cl-ppcre
                  :trivia
                  :trivia.ppcre))
  (load "./file.lisp")
  (defpackage :day4
    (:use :common-lisp)
    (:use :file)
    (:use :cl-arrows)
    (:use :iterate)
    (:use :metabang-bind)
    (:use :cl-ppcre)
    (:use :trivia)
    (:use :trivia.ppcre))
  (in-package :day4))

#+nil
(file-lines 4)

(defun is-valid (passport)
  (>= (count-if (lambda (entry) (member (car (split ":" entry))
                                        '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")
                                        :test #'string-equal))
                (remove-duplicates passport
                                   :test (lambda (one other)
                                           (string-equal (car (split ":" one))
                                                         (car (split ":" other))))))
      7))

(defun part-1 ()
  (iter
    (with current-passport = '())
    (for line in (file-lines 4))
    (setf current-passport (concatenate 'list current-passport (split " " line)))
    (when (string-equal line "")
      (counting (is-valid current-passport) into total-count)
      (setf current-passport (list)))
    (finally
     (return (+ total-count (if (is-valid current-passport) 1 0))))))

;; Wrong: 181
;; Wrong: 250

(defun valid-values (passport)
  (every (lambda (entry)
           (match entry
             ((ppcre "(.*):(.*)"
                     code value)
              (match code
                ("byr" (and (= (length value) 4)
                            (every #'digit-char-p value)
                            (>= (read-from-string value) 1920)
                            (<= (read-from-string value) 2002)))
                ("iyr" (and (= (length value) 4)
                            (every #'digit-char-p value)
                            (>= (read-from-string value) 2010)
                            (<= (read-from-string value) 2020)))
                ("eyr" (and (= (length value) 4)
                            (every #'digit-char-p value)
                            (>= (read-from-string value) 2020)
                            (<= (read-from-string value) 2030)))
                ("hgt" (match value
                         ((ppcre "([0-9]+)cm"
                                 (read height))
                          (and (>= height 150)
                               (<= height 193)))
                         ((ppcre "([0-9]+)in"
                                 (read height))
                          (and (>= height 59)
                               (<= height 76)))))
                ("hcl" (and (char= #\# (elt value 0))
                            (every (lambda (char) (member char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f)
                                                          :test #'char-equal))
                                   (subseq value 1))))
                ("ecl" (member value '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string-equal))
                ("pid" (and (= (length value) 9)
                            (every #'digit-char-p value)))
                ("cid" t)))))
         passport))

(defun part-2 ()
  (iter
    (with current-passport = '())
    (for line in (file-lines 4))
    (setf current-passport (concatenate 'list current-passport (split " " line)))
    (when (string-equal line "")
      (counting (and (is-valid current-passport) (valid-values current-passport)) into total-count)
      (setf current-passport (list)))
    (finally
     (return (+ total-count (if (and (is-valid current-passport) (valid-values current-passport)) 1 0))))))

;; Wrong 110
