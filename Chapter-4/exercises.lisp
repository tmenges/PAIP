(defun permutations (a-list)
  (format t "a-list: ~a~%" a-list)
  (cond ((null a-list) nil)
        ((= (length a-list) 1) a-list)
        ((= (length a-list) 2) (list (list (first a-list) (second a-list))
                                     (list (second a-list) (first a-list))))
        (t (let ((result nil))
             (dolist (item a-list)
               (let ((sub-list (remove item a-list)))
                 (format t "item: ~a, sub-list: ~a~%" item sub-list)
                 (setf result (append
                               (mapcar #'(lambda (lst)
                                           (format t "(cons ~a ~a)~%" item lst)
                                           (cons item lst))
                                       (permutations sub-list))
                               result))))
             result))))

(defun permutations-ans (bag)
  "Return a list of all the permutations of the input."
  (if (null bag)
      '(())
      ;; Otherwise, take an element out of the bag.
      ;; Generate all permutations of the remaining elements.
      ;; Add the element to the front of each of these.
      ;; Do this for all possible elements to generate all permutations.
      (mapcan #'(lambda (e)
                  (mapcar #'(lambda (p) (cons e p))
                          (permutations-ans
                           (remove e bag :count 1 :test #'eq))))
              bag)))

;'(a b c d)
;'(a b d c)
;'(a c b d)
;'(a c d b)
;'(a d b c)
;'(a d c b)

;'(b a c d)
;'(b a d c)
;'(b c a d)
;'(b c d a)
;'(b d a c)
;'(b d c a)

