(defstruct name
  first
  (middle nil)
  last)

(defun length1 (list)
  (let ((len 0))
    (dolist (element list)
      (incf len))
    len))

(defun length1.1 (list)
  (let ((len 0))
    (dolist (element list len)
      (incf len))))
nnn
(defun length2 (list)
  (let ((len 0))
    (mapc #'(lambda (element)
              (incf len))
          list)
    len))

(defun length3 (list)
  (do ((len 0 (+ len 1))
       (l list (rest l)))
      ((null l) len)))

(defun length4 (list)
  (loop for element in list
        count t))

(defun length5 (list)
  (loop for element in list
        summing 1))

(defun length6 (list)
  (loop with len = 0
        until (null list)
        for element = (pop list)
        do (incf len)
        finally (return len)))

(defun length7 (list)
  (count-if #'true list))

(defun true (x) t)

(defun length8 (list)
  (if (null list)
      0
      (+ 1 (position-if #'true list :from-end t))))

(defun length9 (list)
  (if (null list)
      0
      (+ 1 (length9 (rest list)))))

(defun length10 (list)
  (length10-aux list 0))

(defun length10-aux (sublist len-so-far)
  (if (null sublist)
      len-so-far
      (length10-aux (rest sublist) (+ 1 len-so-far))))

(defun length11 (list &optional (len-so-far 0))
  (if (null list)
      len-so-far
      (length11 (rest list) (+ 1 len-so-far))))

(defun length12 (the-list)
  (labels
      ((length13 (list len-so-far)
         (if (null list)
             len-so-far
             (length13 (rest list) (+ 1 len-so-far)))))
    (length13 the-list 0)))

(defun average (numbers)
  (if (null numbers)
      (progn
        (cerror "Use 0 as the average."
                "Average of the empty list is undefined.")
        0)
      (/ (reduce #'+ numbers)
         (length numbers))))


(defun sqr (x)
  (assert (numberp x) (x))
  (* x x))

(defun bank-account (balance)
  "Open a bank account with the given balance."
  #'(lambda (action amount)
      (case action
        (deposit (setf balance (+ balance amount)))
        (withdraw (setf balance (- balance amount))))))

(setf a 'global-a)
(defvar *b* 'global-b)

(defun fn () *b*)

(let ((a 'local-a)
      (*b* 'local-b))
  (list a *b* (fn) (symbol-value 'a) (symbol-value '*b*)))

(defun length-r (list)
  (reduce #'(lambda (acc v) (declare (ignore v)) (+ acc 1)) list :initial-value 0))

(defun add-one (acc v)
  (+ 1 acc))
