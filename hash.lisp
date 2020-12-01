(eval-when (:execute :load-toplevel :compile-toplevel)
  (ql:quickload "ironclad")
  (ql:quickload "flexi-streams")

  (defpackage :hash
    (:use :common-lisp)
    (:use :ironclad)
    (:export md5))

  (in-package :hash))

(defun md5 (str)
  (byte-array-to-hex-string
   (digest-sequence :md5 (flexi-streams:string-to-octets str))))
