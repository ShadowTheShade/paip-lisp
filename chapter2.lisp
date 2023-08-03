;; Chapter 2

;; 2.2 A Straightforward Solution

;; Random selection from a list

(defun random-elt (choices)
  "Choose a random element belonging to a list."
  (elt choices (random (length choices))))

(defun one-of (set)
  "Pick one element from set, and make a list of it."
  (list (random-elt set)))

;; Basic sentence creation

(defun sentence ()    (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Noun)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article ()     (one-of '(the a)))
(defun Noun ()        (one-of '(man ball woman table)))
(defun Verb ()        (one-of '(hit took saw liked)))

;; Adding multiple adjectives and preposition phrases

(defun Adj* ()
  (if (= (random 2) 0)
      nil
      (append (Adj) (Adj*))))

(defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))
(defun PP () (append (Prep) (noun-phrase)))
(defun Adj () (one-of '(big little blue green adiabatic)))
(defun Prep () (one-of '(to in by with on)))



(defun PP* ()
  (if (= (random 2) 0)
      nil
      (append (PP) (PP*))))

;; 2.3 A Rule-Based Solution

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "Simple grammar for english.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially simple grammar.")

(assoc 'sentence *grammar*)

(sentence)

(defun rule-lhs (rule)
  "The left-had side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right hand side of a rule"
  (rest (rest rule)))

(defun rewrites (category)
  "Returns a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  "Generate a random sentence or phrase."
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

;; if version

(defun generate (phrase)
  "Generate a random sentence or a phrase"
  (if (listp phrase)
      (mappend #'generate phrase)
      (let ((val (rewrites phrase)))
        (if val
            (generate (random-elt val))
            (list phrase)))))

;; Exercise 2.1

(defun generate (phrase)
  "Generates a random sentence or phrase using cond, but only uses rewrites once"
  (let ((choices (rewrites phrase)))
    (cond ((listp phrase)
           (mappend #'generate phrase))
          (choices
           (generate (random-elt choices)))
          (t (list phrase)))))

;; Exercise 2.2

(defun generate (phrase)
  "Generates a random sentence or phrase by explicitly checking what is input"
  (if (listp phrase)
      (mappend #'generate phrase)
      (cond ((assoc phrase *grammar*)
             (generate (random-elt (rewrites phrase))))
            (t (list phrase)))))

(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue yellow tall)
    (Article -> the a one)
    (Name -> Pat Dalia Anthony Biagio)
    (Noun -> man ball woman table)
    (Verb -> hit saw took liked)
    (Pronoun -> he she it these those that)))

(setf *grammar* *bigger-grammar*)

;; 2.6 Reusing Data for Several Programs

(defun generate-tree (phrase)
  "Generates a random sentende or phrase, but with a tree description"
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))


(defun combine-all (xlist ylist)
  "Returns a list of lists formed by appending a y to an x. (All permutations)"
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append x y)) xlist))
           ylist))

(defun generate-all (phrase)
  "Generates a list of all possible expansions of the desired phrase"
  (cond ((null phrase) (list nil))
        ((listp phrase)
         (combine-all (generate-all (first phrase))
                      (generate-all (rest phrase))))
        ((rewrites phrase)
         (mappend #'generate-all (rewrites phrase)))
        (t (list (list phrase)))))

;; 2.7 Exercises

;; Exercise 2.3

(defparameter *grammatico-italiano-simplici*
  '((frase -> (frase-nome frase-verbo))
    (frase-nome -> (Clausola Nome))
    (frase-verbo -> (Verbo frase-nome))
    (Clausola -> la il le)
    (Verbo -> frappa saluta vedi)
    (Nome -> personna bambino genti)))

;; Exercise 2.4

(defun cross-product (fn xvec yvec)
  "Perform the cross product based on a function on two vectors."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (funcall fn x y)) xvec))
           yvec))


(defun combine-all-cross (xlist ylist)
  "Returns a list of the cartesian products betweeen xlist and ylist"
  (cross-product #'list xlist ylist))
