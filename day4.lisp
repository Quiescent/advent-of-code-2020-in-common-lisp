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

(defun part-1 ()
  (iter
    (with current-passport = '())
    (for line in (file-lines 4))
    (setf current-passport (concatenate 'list current-passport (split " " line)))
    (when (string-equal line "")
      (counting (>= (count-if (lambda (entry) (member (car (split ":" entry))
                                                        '("byr"
                                                          "iyr"
                                                          "eyr"
                                                          "hgt"
                                                          "hcl"
                                                          "ecl"
                                                          "pid")
                                                        :test #'string-equal))
                                (print (remove-duplicates current-passport
                                                    :test (lambda (one other)
                                                            (string-equal (car (split ":" one))
                                                                          (car (split ":" other)))))))
                    7) into total-count)
      (setf current-passport (list)))
    (finally
     (return (+ total-count (if (>= (count-if (lambda (entry) (member (car (split ":" entry))
                                                        '("byr"
                                                          "iyr"
                                                          "eyr"
                                                          "hgt"
                                                          "hcl"
                                                          "ecl"
                                                          "pid")
                                                        :test #'string-equal))
                                (print (remove-duplicates current-passport
                                                    :test (lambda (one other)
                                                            (string-equal (car (split ":" one))
                                                                          (car (split ":" other)))))))
                                    7)
                                1
                                0))))))

;; Wrong: 181
;; Wrong: 250

(defun part-2 ()
  (iter
    (with current-passport = '())
    (for line in (file-lines 4))
    (setf current-passport (concatenate 'list current-passport (split " " line)))
    (when (string-equal line "")
      (for valid = (and (>= (count-if (lambda (entry) (member (car (split ":" entry))
                                                  '("byr"
                                                    "iyr"
                                                    "eyr"
                                                    "hgt"
                                                    "hcl"
                                                    "ecl"
                                                    "pid")
                                                  :test #'string-equal))
                                   (remove-duplicates current-passport
                                                      :test (lambda (one other)
                                                              (string-equal (car (split ":" one))
                                                                            (car (split ":" other))))))
                         7)
                     (every (lambda (entry) (match entry
                                         ((ppcre "(.*):(.*)"
                                                 code value)
                                          (format t "(cons code value): ~a~%" (cons code value))
                                          (print (match code
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
                                                         (every (lambda (char) (member char
                                                                                  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f)
                                                                                  :test #'char-equal))
                                                                (subseq value 1))))
                                             ("ecl" (member value '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string-equal))
                                             ("pid" (and (= (length value) 9)
                                                         (every #'digit-char-p value)))
                                             ("cid" t))))))
                            (remove-duplicates current-passport
                                               :test (lambda (one other)
                                                       (string-equal (car (split ":" one))
                                                                     (car (split ":" other))))))))
      (format t "valid: ~a~%" valid)
      (counting valid into total-count)
      (setf current-passport (list)))
    (finally
     (return (+ total-count (if (and (>= (count-if (lambda (entry) (member (car (split ":" entry))
                                                  '("byr"
                                                    "iyr"
                                                    "eyr"
                                                    "hgt"
                                                    "hcl"
                                                    "ecl"
                                                    "pid")
                                                  :test #'string-equal))
                                   (remove-duplicates current-passport
                                                      :test (lambda (one other)
                                                              (string-equal (car (split ":" one))
                                                                            (car (split ":" other))))))
                         7)
                     (every (lambda (entry) (match entry
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
                                                        (every (lambda (char) (member char
                                                                                 '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f)
                                                                                 :test #'char-equal))
                                                               (subseq value 1))))
                                            ("ecl" (member value '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string-equal))
                                            ("pid" (and (= (length value) 9)
                                                        (every #'digit-char-p value)))
                                            ("cid" t)))))
                            (remove-duplicates current-passport
                                               :test (lambda (one other)
                                                       (string-equal (car (split ":" one))
                                                                     (car (split ":" other)))))))
                                1
                                0))))))

;; Wrong 110
