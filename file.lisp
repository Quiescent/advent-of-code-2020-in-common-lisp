(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :iterate)
  (ql:quickload :cl-ppcre)
  (ql:quickload :arrow-macros)

  (defpackage :file
    (:use :common-lisp)
    (:use :iter)
    (:use :arrow-macros)
    (:export file-lines)
    (:export line-numbers)
    (:export file-string)
    (:export file-lines-for))

  (in-package :file))

(defun file-lines (day)
  "Produce the lines in the file `day-DAY.in`."
  (with-open-file (f (format nil "day-~a.in" day))
    (iter
      (for line = (ignore-errors (read-line f)))
      (while line)
      (collecting line))))

(defun file-string (day)
  "Produce entire file as a string."
  (with-open-file (f (format nil "day-~a.in" day))
    (-<> (make-string (file-length f))
      (progn
        (read-sequence <> f)
        <>))))

(defun line-numbers (day)
  "Produce the numbers on each line of `day-DAY.in' as a list of lists."
  (->> (file-lines day)
    (mapcar (lambda (line) (-<> (cl-ppcre:split "[^-0-9]" line)
                             (remove "" <> :test #'string-equal)
                             (mapcar #'read-from-string <>))))))

(defun file-lines-for (file)
  "Produce the lines in the file `day-DAY.in`."
  (with-open-file (f file)
    (iter
      (for line = (ignore-errors (read-line f)))
      (while line)
      (collecting line))))
