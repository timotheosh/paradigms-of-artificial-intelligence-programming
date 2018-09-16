;; Exercise 1.1
;; Define a version of last-name that handles "Rex Morgan MD,"
;; "Morton Downey, Jr.," and whatever other cases you can think of.

;; The key is the recursive resend the last name if it ends with one
;; of the *suffixes*. Common Lisp has a function for this called
;; 'butlast'. However some one on Stack Overflow demonstrated a
;; home-grown butlast.


(defparameter *suffixes*
  '(Jr. MD Sr.))

(defun last-name (name)
  "Select the last name from a name represented in a list."
  (if (member (first (last name)) *suffixes*)
      (last-name (butlast name))
      (first (last name))))


;; Exercise 1.2
;; Write a function to exponentiate, or raise a number to an integer
;; power. For example: (power 3 2 ) = 3^2 = 9.

(defun my-power (n e)
  "Returns the number n to e power."
  (if (= e 1)
      n
      (* n (my-power n (- e 1)))))

;; Book's solution.
(defun power (x n)
  "Power raises x to the nth power. N must be an integer >= 0.
   This executes in log n time, because of the check for even n."
  (cond (( = n 0) 1)
        ((evenp n) (expt (power x (/ n 2)) 2))
        (t (* x (power x (- n 1))))))

;; Exercise 1.3
;; Write a function that counts the number of atoms in an
;; expression. For example: (count-atoms '(a (b) c)) = 3. Notice that
;; there is something of an ambiguity in this: should (a nil c) count
;; as three atoms, or as two, because it is equivalent to (a () c)?

;; Seems like a definition to flatten a list would be in order for
;; this exercise. From:
;; https://stackoverflow.com/questions/2680864/how-to-remove-nested-parentheses-in-lisp
(defun flatten (L)
  "Converts a list to single level."
  (if (null L)
      nil
      (if (atom (first L))
          (cons (first L) (flatten (rest L)))
          (append (flatten (first L)) (flatten (rest L))))))

(defun count-atoms (lst)
  (length (flatten lst)))

;; Guess I am suppose to eliminate nil atoms
;; Here is the book's solution.
(defun count-atoms (exp)
  "Return the total number of non-nil atoms in the expression."
  (cond ((null exp) 0)
        ((atom exp) 1)
        (t (+ (count-atoms (first exp))
              (count-atoms (rest exp))))))

;; Exercise 1.4
;; Write a function that counts the number of times an expression
;; occurs anywhere within another expression.
;; Example: (count-anywhere 'a '(a ( ( a ) b) a)) => 3.
(defun my-count-anywhere (x y)
  (length
   (remove nil
           (loop
              for i in (flatten y)
              collect (equalp x i)))))

;; Book solution
(defun count-anywhere (item tree)
  "Count the times item appears anywhere within tree."
  (cond ((eql item tree) 1)
        ((atom tree) 0)
        (t (+ (count-anywhere item (first tree))
              (count-anywhere item (rest tree))))))

;; Exercise 1.5
;; Write a function to compute the dot product of two sequences of
;; numbers, represented as lists. The dot product is computed by
;; multiplying corresponding elements and then adding up the
;; resulting products.
;; Example: (dot-product '(10 20) '(3 4 )) = 10 x 3 + 20 x 4 = 110
(defun my-dot-product (x y)
  (apply #'+ (mapcar #'* x y)))


;; Book versions, one of which was identical to mine.
(defun dot-product (a b)
  "Compute the mathematical dot product of two vectors."
  (if (or (null a) (null b))
      0
      (+ (* (first a) (first b))
         (dot-product (rest a) (rest b)))))

(defun dot-product (a b)
  "Compute the mathematical dot product of two vectors."
  (let ((sum 0))
    (dotimes (i (length a))
      (incf sum (* (elt a i) (elt b i))))
    sum))

(defun dot-product (a b)
  "Compute the mathematical dot product of two vectors."
  (apply #'+ (mapcar #'* a b)))
