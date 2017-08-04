;; Exercise 1.1
;; Define a version of last-name that handles "Rex Morgan MD,"
;; "Morton Downey, Jr.," and whatever other cases you can think of.

;; The key is the recursive resend the last name if it ends with one
;; of the *suffixes*. Common Lisp has a function for this called
;; 'butlast'. However some one on Stack Overflow demonstrated a
;; home-grown butlast.

(defun my-butlast (list)
  (loop for l on list
     while (rest l)
     collect (first l)))

(defparameter *suffixes*
  '(Jr. MD Sr.))

(defun last-name (name)
  "Select the last name from a name represented in a list."
  (if (member (first (last name)) *suffixes*)
      (last-name (my-butlast name))
      (first (last name))))


;; Exercise 1.2
;; Write a function to exponentiate, or raise a number to an integer
;; power. For example: (power 3 2 ) = 3^2 = 9.

(defun my-power (n e)
  "Returns the number n to e power."
  (if (= e 1)
      n
      (my-power (* n n) (- e 1))))

(defun power (x n)
  "Power raises x to the nth power. N must be an integer >= 0.
   This executes in log n time, because of the check for even n."
  (cond (( = n 0) 1)
        ((evenp n) (expt (power x (/ n 2)) 2))
        (t (* x (power x (- n 1))))))
