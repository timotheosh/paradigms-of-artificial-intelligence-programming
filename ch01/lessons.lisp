(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General Colonel))

(defun last-name (name)
  "Select the last name from a name represented in a list."
  (first (last name)))

(defun first-name (name)
  "Select the first name from a name represented as a list."
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))

(setf names '((John Q Public) (Malcolm X)
              (Admiral Grace Murray Hopper) (Spot)
              (Aristotle) (A A Milne) (Z Z Top)
              (Sir Larry Olivier) (Miss Scarlet)))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (if (null the-list)
      nil
      (append (funcall fn (first the-list))
              (mappend fn (rest the-list)))))

(defun self-and-double (x) (list x (+ x x)))

(defun number-and-negation (x)
  "If x is a number, return a list of x and -x."
  (when (numberp x)
    (list x (- x))))

(defun numbers-and-negations (input)
  "Given a list, return only the numbers and their negations."
  (mappend #'number-and-negation input))
