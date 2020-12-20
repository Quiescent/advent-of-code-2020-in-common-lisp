(eval-when (:execute :compile-toplevel :load-toplevel)
  (ql:quickload '(:iterate
                  :cl-arrows
                  :cl-ppcre
                  :metabang-bind
                  :anaphora
                  :trivia
                  :trivia.ppcre
                  :cl-heap))
  (load "./file.lisp")
  (defpackage :day20
    (:use :common-lisp)
    (:use :file)
    (:use :iterate)
    (:use :cl-arrows)
    (:use :cl-ppcre)
    (:use :metabang-bind)
    (:use :anaphora)
    (:use :trivia)
    (:use :trivia.ppcre)
    (:use :cl-heap))
  (in-package :day20))

#+nil
(file-lines 20)

(defun up (tile)
  (match tile
    ((list _ _ _ _ (list up _ _ _) _) up)))

(defun right (tile)
  (match tile
    ((list _ _ _ _ (list _ right _ _) _) right)))

(defun down (tile)
  (match tile
    ((list _ _ _ _ (list _ _ down _) _) down)))

(defun left (tile)
  (match tile
    ((list _ _ _ _ (list _ _ _ left) _) left)))

(defun raw-tile (tile)
  (match tile
    ((list _ _ _ _ (list _ _ _ _) raw-tile) raw-tile)))

(defun works (tile x y grid)
  (bind (((id rot flip-vert flip-hor (u r d l) raw-tile) tile)
         (up-tile          (gethash (cons x (1- y)) grid))
         (right-tile       (gethash (cons (1+ x) y) grid))
         (down-tile        (gethash (cons x (1+ y)) grid))
         (left-tile        (gethash (cons (1- x) y) grid))
         (orientations     (list )))
    ;; Standard orientation
    (when (and (or (null up-tile)
              (string-equal u (down up-tile)))
          (or (null right-tile)
              (string-equal r (left right-tile)))
          (or (null down-tile)
              (string-equal d (up down-tile)))
          (or (null left-tile)
              (string-equal l (right left-tile))))
      (push tile orientations))
    ;; Rot: 90
    (bind (((u r d l) (list r (reverse d) l (reverse u))))
      (when (and (or (null up-tile)
                (string-equal u (down up-tile)))
            (or (null right-tile)
                (string-equal r (left right-tile)))
            (or (null down-tile)
                (string-equal d (up down-tile)))
            (or (null left-tile)
                (string-equal l (right left-tile))))
        (push (list id 90 nil nil (list u r d l) raw-tile) orientations))
      ;; Rot: 180 (Hack!  Rotate the block rotated by 90! XD)
      (bind (((u r d l) (list r (reverse d) l (reverse u))))
        (when (and (or (null up-tile)
                  (string-equal u (down up-tile)))
              (or (null right-tile)
                  (string-equal r (left right-tile)))
              (or (null down-tile)
                  (string-equal d (up down-tile)))
              (or (null left-tile)
                  (string-equal l (right left-tile))))
          (push (list id 180 nil nil (list u r d l) raw-tile) orientations))
        ;; Rot: 270 (Hack!  Rotate the block rotated by 180! XD)
        (bind (((u r d l) (list r (reverse d) l (reverse u))))
          (when (and (or (null up-tile)
                    (string-equal u (down up-tile)))
                (or (null right-tile)
                    (string-equal r (left right-tile)))
                (or (null down-tile)
                    (string-equal d (up down-tile)))
                (or (null left-tile)
                    (string-equal l (right left-tile))))
            (push (list id 270 nil nil (list u r d l) raw-tile) orientations)))))
    ;; Flip over vertical axis
    (bind (((u r d l) (list (reverse u) l (reverse d) r)))
      (when (and (or (null up-tile)
                (string-equal u (down up-tile)))
            (or (null right-tile)
                (string-equal r (left right-tile)))
            (or (null down-tile)
                (string-equal d (up down-tile)))
            (or (null left-tile)
                (string-equal l (right left-tile))))
        (push (list id 0 t nil (list u r d l) raw-tile) orientations))
      ;; Then do rotations...
      ;; Rot: 90
      (bind (((u r d l) (list r (reverse d) l (reverse u))))
        (when (and (or (null up-tile)
                  (string-equal u (down up-tile)))
              (or (null right-tile)
                  (string-equal r (left right-tile)))
              (or (null down-tile)
                  (string-equal d (up down-tile)))
              (or (null left-tile)
                  (string-equal l (right left-tile))))
          (push (list id 90 t nil (list u r d l) raw-tile) orientations))
        ;; Rot: 180 (Hack!  Rotate the block rotated by 90! XD)
        (bind (((u r d l) (list r (reverse d) l (reverse u))))
          (when (and (or (null up-tile)
                    (string-equal u (down up-tile)))
                (or (null right-tile)
                    (string-equal r (left right-tile)))
                (or (null down-tile)
                    (string-equal d (up down-tile)))
                (or (null left-tile)
                    (string-equal l (right left-tile))))
            (push (list id 180 t nil (list u r d l) raw-tile) orientations))
          ;; Rot: 270 (Hack!  Rotate the block rotated by 180! XD)
          (bind (((u r d l) (list r (reverse d) l (reverse u))))
            (when (and (or (null up-tile)
                      (string-equal u (down up-tile)))
                  (or (null right-tile)
                      (string-equal r (left right-tile)))
                  (or (null down-tile)
                      (string-equal d (up down-tile)))
                  (or (null left-tile)
                      (string-equal l (right left-tile))))
              (push (list id 270 t nil (list u r d l) raw-tile) orientations))))))
    ;; Flip over horizontal axis
    (bind (((u r d l) (list d (reverse r) u (reverse l))))
      (when (and (or (null up-tile)
                (string-equal u (down up-tile)))
            (or (null right-tile)
                (string-equal r (left right-tile)))
            (or (null down-tile)
                (string-equal d (up down-tile)))
            (or (null left-tile)
                (string-equal l (right left-tile))))
        (push (list id 0 nil t (list u r d l) raw-tile) orientations))
      ;; Then do rotations...
      ;; Rot: 90
      (bind (((u r d l) (list r (reverse d) l (reverse u))))
        (when (and (or (null up-tile)
                  (string-equal u (down up-tile)))
              (or (null right-tile)
                  (string-equal r (left right-tile)))
              (or (null down-tile)
                  (string-equal d (up down-tile)))
              (or (null left-tile)
                  (string-equal l (right left-tile))))
          (push (list id 90 nil t (list u r d l) raw-tile) orientations))
        ;; Rot: 180 (Hack!  Rotate the block rotated by 90! XD)
        (bind (((u r d l) (list r (reverse d) l (reverse u))))
          (when (and (or (null up-tile)
                    (string-equal u (down up-tile)))
                (or (null right-tile)
                    (string-equal r (left right-tile)))
                (or (null down-tile)
                    (string-equal d (up down-tile)))
                (or (null left-tile)
                    (string-equal l (right left-tile))))
            (push (list id 180 nil t (list u r d l) raw-tile) orientations))
          ;; Rot: 270 (Hack!  Rotate the block rotated by 180! XD)
          (bind (((u r d l) (list r (reverse d) l (reverse u))))
            (when (and (or (null up-tile)
                      (string-equal u (down up-tile)))
                  (or (null right-tile)
                      (string-equal r (left right-tile)))
                  (or (null down-tile)
                      (string-equal d (up down-tile)))
                  (or (null left-tile)
                      (string-equal l (right left-tile))))
              (push (list id 270 nil t (list u r d l) raw-tile) orientations))))))
    orientations))

(defun arrange-tiles (tiles grid spots-taken)
  (if (null tiles)
      grid
      (iter
        (for tile in tiles)
        (thereis
         (iter outer
           (for (x . y) in spots-taken)
           (for up-free    = (null (gethash (cons x (1- y)) grid)))
           (awhen (and up-free (works tile x (1- y) grid))
             (iter
               (for orientation in it)
               (setf (gethash (cons x (1- y)) grid) orientation)
               (aif (arrange-tiles (remove tile tiles) grid (cons (cons x (1- y)) spots-taken))
                    (in outer (leave it))
                    (remhash (cons x (1- y)) grid))))
           (for right-free = (null (gethash (cons (1+ x) y) grid)))
           (awhen (and right-free (works tile (1+ x) y grid))
             (iter
               (for orientation in it)
               (setf (gethash (cons (1+ x) y) grid) orientation)
               (aif (arrange-tiles (remove tile tiles) grid (cons (cons (1+ x) y) spots-taken))
                    (in outer (leave it))
                    (remhash (cons (1+ x) y) grid))))
           (for down-free  = (null (gethash (cons x (1+ y)) grid)))
           (awhen (and down-free (works tile x (1+ y) grid))
             (iter
               (for orientation in it)
               (setf (gethash (cons x (1+ y)) grid) orientation)
               (aif (arrange-tiles (remove tile tiles) grid (cons (cons x (1+ y)) spots-taken))
                    (in outer (leave it))
                    (remhash (cons x (1+ y)) grid))))
           (for left-free  = (null (gethash (cons (1- x) y) grid)))
           (awhen (and left-free (works tile (1- x) y grid))
             (iter
               (for orientation in it)
               (setf (gethash (cons (1- x) y) grid) orientation)
               (aif (arrange-tiles (remove tile tiles) grid (cons (cons (1- x) y) spots-taken))
                    (in outer (leave it))
                    (remhash (cons (1- x) y) grid)))))))))

(defun part-1 ()
  (bind ((tiles (->> (file-string 20)
                  (split (create-scanner '(:sequence #\Newline #\Newline)))
                  (mapcar (lambda (raw-tile)
                            (match (split (create-scanner #\Newline) raw-tile)
                              ((list* id-row rest)
                               (list (-<> (split "[^0-9]" id-row)
                                       (remove "" <> :test #'string-equal)
                                       (car <>)
                                       (read-from-string <>))
                                     0
                                     nil
                                     nil
                                     (bind ((blocks (map 'vector #'identity rest))
                                            (x-dim  (length (aref blocks 0)))
                                            (top    (aref blocks 0))
                                            (bottom (aref blocks (1- (length blocks))))
                                            (l-col   (iter
                                                       (for y from 0 below (length blocks))
                                                       (collecting (-> (aref blocks y) (aref 0))
                                                                   :result-type 'string)))
                                            (r-col  (iter
                                                      (for y from 0 below (length blocks))
                                                      (collecting (-> (aref blocks y) (aref (1- x-dim)))
                                                                  :result-type 'string))))
                                       (list top r-col bottom l-col))
                                     (map 'vector #'identity rest))))))))
         (grid (make-hash-table :test #'equal)))
    (setf (gethash (cons 0 0) grid) (car tiles))
    (arrange-tiles (cdr tiles)
                   grid
                   (list (cons 0 0)))
    (iter
      (for (key value) in-hashtable grid)
      (minimizing (car key) into min-x)
      (maximizing (car key) into max-x)
      (minimizing (cdr key) into min-y)
      (maximizing (cdr key) into max-y)
      (finally
       (return
         (* (car (gethash (cons min-x min-y) grid))
            (car (gethash (cons max-x min-y) grid))
            (car (gethash (cons min-x max-y) grid))
            (car (gethash (cons max-x max-y) grid))))))))

(defun transpose (matrix)
  (bind ((x-dim (length (aref matrix 0)))
         (y-dim (length matrix))
         (new-matrix (iter
                       (for y from 0 below x-dim)
                       (collecting
                        (iter
                          (for x from 0 below y-dim)
                          (collecting #\. :result-type 'string))
                        :result-type 'vector))))
    (iter
      (for row in-vector matrix)
      (for x from 0)
      (iter
        (for value in-string row)
        (for y from 0)
        (setf (-> (aref new-matrix y) (aref x)) value))
      (finally (return new-matrix)))))

#+nil
(transpose (vector "12345" "67890"))

(defun rot-90 (block)
  (-> (map 'vector #'reverse block)
    (transpose)))

(defun rot-180 (block)
  (-> (rot-90 block)
    (rot-90)))

(defun rot-270 (block)
  (-> (rot-180 block)
    (rot-90)))

#+nil
(rot-90 (vector "123" "456" "789"))

(defun part-2 ()
  (bind ((tiles (->> (file-string 20)
                  (split (create-scanner '(:sequence #\Newline #\Newline)))
                  (mapcar (lambda (raw-tile)
                            (match (split (create-scanner #\Newline) raw-tile)
                              ((list* id-row rest)
                               (list (-<> (split "[^0-9]" id-row)
                                       (remove "" <> :test #'string-equal)
                                       (car <>)
                                       (read-from-string <>))
                                     0
                                     nil
                                     nil
                                     (bind ((blocks (map 'vector #'identity rest))
                                            (x-dim  (length (aref blocks 0)))
                                            (top    (aref blocks 0))
                                            (bottom (aref blocks (1- (length blocks))))
                                            (l-col   (iter
                                                       (for y from 0 below (length blocks))
                                                       (collecting (-> (aref blocks y) (aref 0))
                                                                   :result-type 'string)))
                                            (r-col  (iter
                                                      (for y from 0 below (length blocks))
                                                      (collecting (-> (aref blocks y) (aref (1- x-dim)))
                                                                  :result-type 'string))))
                                       (list top r-col bottom l-col))
                                     (map 'vector #'identity rest))))))))
         (grid (make-hash-table :test #'equal)))
    (setf (gethash (cons 0 0) grid) (car tiles))
    (arrange-tiles (cdr tiles)
                   grid
                   (list (cons 0 0)))
    (iter
      (for (key value) in-hashtable grid)
      (format t "(list key value): ~a~%" (list key value))
      (minimizing (car key) into min-x)
      (maximizing (car key) into max-x)
      (minimizing (cdr key) into min-y)
      (maximizing (cdr key) into max-y)
      (finally
       (return
         (bind ((x-count      (1+ (- max-y min-y)))
                (x-dim        (* 8 x-count))
                (y-count      (1+ (- max-x min-x)))
                (y-dim        (* 8 y-count))
                (painted-grid (iter
                                (for x from 0 below x-dim)
                                (collecting
                                 (iter
                                   (for y from 0 below y-dim)
                                   (collecting #\. :result-type 'string))
                                 :result-type 'vector))))
           (format t "(list min-x max-x min-y max-y): ~a~%" (list min-x max-x min-y max-y))
           (iter
             (for x from 0 below x-dim)
             (multiple-value-bind (low-res-x inner-x) (floor x 8)
               (iter
                 (for y from 0 below y-dim)
                 (multiple-value-bind (low-res-y inner-y) (floor y 8)
                   (for (id rot flip-vert flip-hor rows raw-tile) =
                        (gethash (cons (+ low-res-x min-x) (+ low-res-y min-y)) grid))
                   (for working-block = raw-tile)
                   (when flip-vert
                     (setf working-block
                           (map 'vector #'reverse working-block)))
                   (when flip-hor
                     (setf working-block
                           (reverse working-block)))
                   (when (= rot 90)
                     (setf working-block
                           (rot-90 working-block)))
                   (when (= rot 180)
                     (setf working-block
                           (rot-180 working-block)))
                   (when (= rot 270)
                     (setf working-block
                           (rot-270 working-block)))
                   (for block = working-block)
                   (setf (-> (aref painted-grid y) (aref x))
                         (-> (aref block (1+ inner-y)) (aref (1+ inner-x))))))))

           ;; 21 monsters found with following code...
           ;;                   # 
           ;; #    ##    ##    ###
           ;;  #  #  #  #  #  #
           ;; 15 hashes in a sea monster...
           (-
            (iter
              (for row in-vector painted-grid)
              (summing
               (iter
                 (for value in-string row)
                 (counting (char= value #\#)))))
            (* 15 21))

           ;; (format t "top-left: ~a~%" (gethash (cons min-x min-y) grid))
           ;; (iter
           ;;   (for row in-vector (raw-tile (gethash (cons min-x min-y) grid)))
           ;;   (format t "~a~%" row))
           ;; (iter
           ;;   (for row in-vector painted-grid)
           ;;   (format t "~a~%" row))
           ;; ;; Standard orientation
           ;; (format t "Standard orientation~%")
           ;; (iter
           ;;   (for row in-vector painted-grid)
           ;;   (for y from 0)
           ;;   (awhen (all-matches "#....##....##....###" row)
           ;;     (format t "found body~%")
           ;;     (when (and (>= y 1)
           ;;                (all-matches "^..................#."
           ;;                             (subseq (aref painted-grid (1- y)) (car it)))
           ;;                (all-matches "^.#..#..#..#..#..#..."
           ;;                             (subseq (aref painted-grid (1+ y)) (car it))))
           ;;       (format t "it: ~a~%" it))))
           ;; ;; Flip over horizontal axis
           ;; (format t "Flip over horizontal axis~%")
           ;; (iter
           ;;   (for row in-vector (reverse painted-grid))
           ;;   (for y from 0)
           ;;   (awhen (all-matches "#....##....##....###" row)
           ;;     (format t "found body~%")
           ;;     (when (and (>= y 1)
           ;;                (all-matches "^..................#."
           ;;                             (subseq (aref painted-grid (1- y)) (car it)))
           ;;                (all-matches "^.#..#..#..#..#..#..."
           ;;                             (subseq (aref painted-grid (1+ y)) (car it))))
           ;;       (format t "it: ~a~%" it))))
           ;; ;; Flip over vertical axis
           ;; (format t "Flip over vertical axis~%")
           ;; (iter
           ;;   (for row in-vector (map 'vector #'reverse painted-grid))
           ;;   (for y from 0)
           ;;   (awhen (all-matches "#....##....##....###" row)
           ;;     (format t "found body~%")
           ;;     (when (and (>= y 1)
           ;;                (all-matches "^..................#."
           ;;                             (subseq (aref painted-grid (1- y)) (car it)))
           ;;                (all-matches "^.#..#..#..#..#..#..."
           ;;                             (subseq (aref painted-grid (1+ y)) (car it))))
           ;;       (format t "it: ~a~%" it))))
           ;; ;; Rotate 90
           ;; (format t "Rotate 90~%")
           ;; (iter
           ;;   (with transformed = (rot-90 painted-grid))
           ;;   (for row in-vector transformed)
           ;;   (for y from 0)
           ;;   (awhen (all-matches "#....##....##....###" row)
           ;;     (format t "found body~%")
           ;;     (when (and (>= y 1)
           ;;                (all-matches "^..................#."
           ;;                             (subseq (aref transformed (1- y)) (car it)))
           ;;                (all-matches "^.#..#..#..#..#..#..."
           ;;                             (subseq (aref transformed (1+ y)) (car it))))
           ;;       (format t "it: ~a~%" it))))
           ;; ;; Rotate 180
           ;; (format t "Rotate 180~%")
           ;; (iter
           ;;   (with transformed = (rot-180 painted-grid))
           ;;   (for row in-vector transformed)
           ;;   (for y from 0)
           ;;   (awhen (all-matches "#....##....##....###" row)
           ;;     (format t "found body~%")
           ;;     (when (and (>= y 1)
           ;;                (all-matches "^..................#."
           ;;                             (subseq (aref transformed (1- y)) (car it)))
           ;;                (all-matches "^.#..#..#..#..#..#..."
           ;;                             (subseq (aref transformed (1+ y)) (car it))))
           ;;       (format t "it: ~a~%" it))))
           ;; ;; Rotate 270
           ;; (format t "Rotate 270~%")
           ;; (iter
           ;;   (for row in-vector (rot-270 painted-grid))
           ;;   (format t "~a~%" row))
           ;; (iter
           ;;   (with transformed = (rot-270 painted-grid))
           ;;   (for row in-vector transformed)
           ;;   (for y from 0)
           ;;   (awhen (all-matches "#....##....##....###" row)
           ;;     (format t "found body~%")
           ;;     (when (and (>= y 1)
           ;;                (all-matches "^..................#."
           ;;                             (subseq (aref transformed (1- y)) (car it)))
           ;;                (all-matches "^.#..#..#..#..#..#..."
           ;;                             (subseq (aref transformed (1+ y)) (car it))))
           ;;       (format t "it: ~a~%" it))))
           ;; ;; Rotate 90 & flip over vertical axis
           ;; (format t "Rotate 90~%")
           ;; (iter
           ;;   (with transformed = (->> (rot-90 painted-grid)
           ;;                         (map 'vector #'reverse)))
           ;;   (for row in-vector transformed)
           ;;   (for y from 0)
           ;;   (awhen (all-matches "#....##....##....###" row)
           ;;     (format t "found body~%")
           ;;     (when (and (>= y 1)
           ;;                (all-matches "^..................#."
           ;;                             (subseq (aref transformed (1- y)) (car it)))
           ;;                (all-matches "^.#..#..#..#..#..#..."
           ;;                             (subseq (aref transformed (1+ y)) (car it))))
           ;;       (format t "it: ~a~%" it))))
           ;; ;; Rotate 180 & flip over vertical axis
           ;; (format t "Rotate 180~%")
           ;; (iter
           ;;   (with transformed = (->> (rot-180 painted-grid)
           ;;                        (map 'vector #'reverse)))
           ;;   (for row in-vector transformed)
           ;;   (for y from 0)
           ;;   (awhen (all-matches "#....##....##....###" row)
           ;;     (format t "found body~%")
           ;;     (when (and (>= y 1)
           ;;                (all-matches "^..................#."
           ;;                             (subseq (aref transformed (1- y)) (car it)))
           ;;                (all-matches "^.#..#..#..#..#..#..."
           ;;                             (subseq (aref transformed (1+ y)) (car it))))
           ;;       (format t "it: ~a~%" it))))
           ;; ;; Rotate 270 & flip over vertical axis
           ;; (format t "Rotate 270~%")
           ;; (iter
           ;;   (with transformed = (->> (rot-270 painted-grid)
           ;;                        (map 'vector #'reverse)))
           ;;   (for row in-vector transformed)
           ;;   (for y from 0)
           ;;   (awhen (all-matches "#....##....##....###" row)
           ;;     (format t "found body~%")
           ;;     (when (and (>= y 1)
           ;;                (all-matches "^..................#."
           ;;                             (subseq (aref transformed (1- y)) (car it)))
           ;;                (all-matches "^.#..#..#..#..#..#..."
           ;;                             (subseq (aref transformed (1+ y)) (car it))))
           ;;       (format t "it: ~a~%" it))))
           ;; ;; Rotate 90 & flip over horizontal axis
           ;; (format t "Rotate 90~%")
           ;; (iter
           ;;   (with transformed = (->> (rot-90 painted-grid)
           ;;                        (reverse)))
           ;;   (for row in-vector transformed)
           ;;   (for y from 0)
           ;;   (awhen (all-matches "#....##....##....###" row)
           ;;     (format t "found body~%")
           ;;     (when (and (>= y 1)
           ;;                (all-matches "^..................#."
           ;;                             (subseq (aref transformed (1- y)) (car it)))
           ;;                (all-matches "^.#..#..#..#..#..#..."
           ;;                             (subseq (aref transformed (1+ y)) (car it))))
           ;;       (format t "it: ~a~%" it))))
           ;; ;; Rotate 180 & flip over horizontal axis
           ;; (format t "Rotate 180~%")
           ;; (iter
           ;;   (with transformed = (->> (rot-180 painted-grid)
           ;;                        (reverse)))
           ;;   (for row in-vector transformed)
           ;;   (for y from 0)
           ;;   (awhen (all-matches "#....##....##....###" row)
           ;;     (format t "found body~%")
           ;;     (when (and (>= y 1)
           ;;                (all-matches "^..................#."
           ;;                             (subseq (aref transformed (1- y)) (car it)))
           ;;                (all-matches "^.#..#..#..#..#..#..."
           ;;                             (subseq (aref transformed (1+ y)) (car it))))
           ;;       (format t "it: ~a~%" it))))
           ;; ;; Rotate 270 & flip over horizontal axis
           ;; (format t "Rotate 270~%")
           ;; (iter
           ;;   (with transformed = (->> (rot-270 painted-grid)
           ;;                        (reverse)))
           ;;   (for row in-vector transformed)
           ;;   (for y from 0)
           ;;   (awhen (all-matches "#....##....##....###" row)
           ;;     (format t "found body~%")
           ;;     (when (and (>= y 1)
           ;;                (all-matches "^..................#."
           ;;                             (subseq (aref transformed (1- y)) (car it)))
           ;;                (all-matches "^.#..#..#..#..#..#..."
           ;;                             (subseq (aref transformed (1+ y)) (car it))))
           ;;       (format t "it: ~a~%" it))))
           ))))))

;; Too high... 1757
;; Too high... 1742
