(setf p '(John Q Small))

  (setf names '((John Q Public) (Malcolm X)
                     (Admiral Grace Murray Hopper) (Spot)
                     (Aristotle) (A A Milne) (Z Z Top)
                     (Sir Larry Olivier) (Miss Scarlet)))

  (defun last-name (name)
    "Select the last name from a name represented in a list"
    (first (last name)))

  (defparameter *titles*
    '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
    "A list of titles that can appear at the start of a name")

  (defun first-name (name)
    "Select the first name from a name represented as a list"
    (if (member (first name) *titles*)
        (first-name (rest name))
        (first name)))

  (mapcar #'last-name names)
  (mapcar #'first-name names)

  (defparameter *titles*
    '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
    "A list of titles that can appear at the start of a name")

  ;; Trace function
  (trace first-name)

  (first-name '(Madam Major General Paula Jones))

  (untrace first-name)

                                          ; Higher Order Functions

  (defun mappend (fn the-list)
    "Apply fn to each element of the list and append the result to each element"
    (apply #'append (mapcar fn the-list)))

  (defun self-and-double (x) (list x (+ x x)))

  (self-and-double 3)

  (mapcar #'self-and-double '(1 10 300))

  (mappend #'self-and-double '(1 10 300))

  (defun number-and-negation (x)
    "If x is a number, return a list of x and -x"
    (if (numberp x)
        (list x (- x)
              nil)))

  (defun numbers-and-negations (input)
    "Given a list, return only the numbers and their negations"
    (mappend #'number-and-negation input))

  ;; Updated mappend
  (defun mappend (fn the-list)
    "Apply fn to each element of list and append the results"
    (if (null the-list)
        nil
        (append (funcall fn (first the-list))
                (mappend fn (rest the-list)))))

  ;; Anonymous functions

  ((lambda (x) (+ x 2)) 4)

  (funcall #'(lambda (x) (+ x 2)) 4)

  ;; Examples
  (mapcar #'(lambda (x) (+ x x)) '(1 2 3 4 5))

  (mappend #'(lambda (l) (list l (reverse l))) '((1 2 3) (a b c)))

  ;; 1.8 Other Data Types

  "a string"

  (length "a string")

  (length "")

  ;; 1.9 Lisp Evaluation Rule
  'John (quote John) ;; Equivalent

  ;; 1.10 Features and Uniqueness of Lisp

  (defun atomprint (exp &optional (depth 0))
    "Print each atom in exp, along with its depth of nesting."
    (if (atom exp)
        (format t "~&ATOM: ~a, DEPTH ~d" exp depth)
        (dolist (element exp)
          (atomprint element (+ depth 1)))))

  (atomprint p)

  ;; 1.11 Exercises

  ;; Exercise 1.1

  (defparameter *end-titles*
    '(MD Jr. Sr.))

  (defun last-name (name)
    "Prints out the last name of a name list, filtering out end titles"
    (if (or (not (rest name)) (member (second name) *end-titles*))
        (first name)
        (last-name (rest name))))

  (setq jimmy '(Jimmy Smalls Jr. Sr.))
  (setq john '(John Waters))

  (last-name jimmy)
  (last-name john)

  ;; Exercise 1.2

  (defun power (base exp)
    "Returns a number base raised to the exp integer power"
    (if (>= exp 1)
        (* base (power base (- exp 1)))
        1))

  (power 3 2)
  (power 6 2)
  (power 5 3)

  ;; Exercise 1.3

  (defun count-atoms (exp &optional (total 0))
    "Counts the atoms in an expression"
    (if (or (rest exp) (first exp))
        (count-atoms (rest exp) (+ total 1))
        total))

(count-atoms '(1 2 a b))
(count-atoms '((1 "a") () (a)))

;; Exercise 1.4
(defun count-anywhere (exp tree)
  "Counts the number of times an expression shows up in another"
  (cond ((eql exp tree) 1)
        ((atom tree) 0)
        (t (+ (count-anywhere exp (first tree))
              (count-anywhere exp (rest tree))))))

(count-anywhere 'a '(a (a b) ((b a) a)))

;; Exercise 1.5
(defun dot-product (v1 v2)
  "Takes the dot product of two vectors"
  (if (first v1)
      (+
      (* (first v1) (first v2))
      (dot-product (rest v1) (rest v2)))
      0))

(dot-product '(10 20) '(3 4))
