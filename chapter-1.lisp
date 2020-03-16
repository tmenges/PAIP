(setf names '((John Q Public)
              (Malcolm X)
              (Admiral Grace Murray Hopper)
              (Spot)
              (Aristotle)
              (A A Milne)
              (Z Z Top)
              (Sir Larry Olivier)
              (Miss Scarlet)))

(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name.")

(defun last-name (name)
  "Select the last name from a name reprsented as a list."
  (first (last name)))

(defun first-name (name)
  "select the first name from a name represented as a list."
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))

