;; Chapter 3 : Overview of Special Forms and Functions

;; 3.1 A Guide to Lisp Style

;; Special Forms

(setq body 1)

;; Functions and macros
(defun function-name (parameters) "documentation" body)
(defmacro macro-name (parameters) "documentation" body)

;; Variables, parameters, constants
(setq initial-value 1)
(setq value 1)

(defvar variable-name initial-value "documentation")
(defparameter parameter-name value "documentation")
(defconstant constant-name value "documentation")

variable-name

;; Structs

(defstruct name
  first
  (middle nil)
  last)

;; defstruct automatically defines:
;; constructor function make-name
;; recognizer predicate name-p
;; accessor functions: name-first, name-middle, name-last
;; middle is nil by default

(setf b (make-name :first 'Anthony :last 'Gagliano))

(name-first b)
(name-middle b)
(name-last b)
(name-p b)
(name-p 'Anthony)
;; Structs are arrays in memory, index 0 is the type, index 1 is first.. etc

;; Conditionals

(if nil
    t)

(setq n 200)

;; Single condition

(when (> n 100)
  (princ "N is large"))

;; Many condition

(defun grade-scheme (grade)
  "Determine the letter grade for a given numeric grade"
  (cond ((< grade 60) 'F)
        ((< grade 70) 'D)
        ((< grade 75) 'C)
        ((< grade 85) 'B)
        ((< grade 95) 'A)
        (t 'A+)))

;; Setf
(setf li '(a b c))

(setf (rest li) nil)

;; let

;; x and y are bound locally

(setq x 5)
(setq y 12)

(let ((x 40)
      (y (+ 1 1)))
  (+ x y))

(+ x y)

;; Anonymous functions

((lambda (x y)
   (+ x y))
 40
 (+ 1 1))

;; let* allows you to use variables already defined in the definition list to define new ones
(let* ((x 6)
       (y (* x x)))
  (+ x y))

;; Exercise 3.1

((lambda (x)
   ((lambda (y)
      (+ x y)) (* x x))) 6)

(defvar alist '(a b c))
(push 'z alist)
(pop alist)

;; Incrementing

(defvar inc 1)
(incf inc)
(decf inc)

;; Infinite Integer Generatior

(let ((x 0))
  (defun int-up ()
    "Every time this function is called, it increases by 1"
    (incf x 1))
  (defun int-down ()
    "Every time this function is called, it is decreased by 1"
    (decf x 1)))

(int-up)
(int-down)

(if 0
    t
    0)

(repeat #'int-up 10)

;; Example using players and scores in a game
(defstruct player
  (score 0)
  (wins 0))

(defun determine-winner (players)
  "Increment the WINS for the player with the highest score"
  (incf (player-wins (first (sort players #'>
                                  :key #'player-score)))))

(defun determine-winner (players)
  "Increment the WINS for the player with the highest score"
  (let ((temp (first (sort players #'> :key #'player-score))))
  (setf (player-wins temp) (+ (player-wins temp) 1))))

;; Repetition special forms and functions

(defun length1 (list)
  "Increment len for every element in the list starting from 0"
  (let ((len 0))
    (dolist (element list)
      (incf len))
    len))

(defun length2 (list)
  "Applies an increment function to every element of the list"
  (let ((len 0))
    (mapc #'(lambda (element)
              (incf len))
          list)
    len))

;; mapc is like mapcar but mapcar returns the values in a list

(defun len3 (list)
  "Increment len by one and shorten list by one every loop until l is null and return len"
  (do ((len 0 (+ len 1))
       (l list (rest l)))
      ((null l) len)))

(len3 '(a b c d))


;; loop specific language

(defun len4 (list)
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

(defun true (x) t)

(defun length7 (list)
  (count-if #'true list))

(defun length8 (list)
  (if (null list)
      0
      (+ 1 (position-if #'true list :from-end t))))

;;Function examples
(mapcar #'- '(1 2 3))
(mapcar #'+ '(1 2) '(10 20))
(mapcar #'+ '(1 2) '(10 20) '(100 200))

(remove 1 '(1 2 3 2 1 0 -1))
(remove 1 '(1 2 3 2 1 0 -1) :key #'abs)
(remove 1 '(1 2 3 2 1 0 -1) :test #'<)
(remove 1 '(1 2 3 2 1 0 -1) :start 4)

(remove-if #'oddp '(1 2 3 2 1 0 -1))
(remove-if-not #'oddp '(1 2 3 2 1 0 -1))
(find-if #'evenp '(1 2 3 2 1 0 -1))

;; Other examples
(setq x '(a b c))
(setq y '(1 2 3))

(every #'oddp y) ; test if every element satisfies a predicate
(some #'oddp y) ; test is some elements satisfy a predicate
(mapcar #'- y) ; apply function to each value and return result
(mapc #'print y) ; apply function to each value

(member 2 y)
(count 'b x)
(delete 1 y) ; omit matching elements
(find 2 y) ; first element that matches
(position 'a x) ; index of the element in the sequence
(reduce #'+ y) ; fold the list applying function to successive elements
(remove 2 y) ; like delete but makes a copy
(substitute 4 2 y) ; replace every matching element with the new one

;; Repetition through recursion
(defun length9 (list)
  (if (null list)
      0
      (+ 1 (length9 (rest list)))))

;; Helper function to avoid bulding a stack frame -> tail-end recursion
(defun length10-aux (sublist len-so-far)
  (if (null sublist)
      len-so-far
      (len10-aux (rest sublist) (+ 1 len-so-far))))

(defun length10 (list)
  (length10-aux list 0))

;; Combining them
(defun length11 (list &optional (len-so-far 0))
  (if (null list)
      len-so-far
      (length11 (rest list) (+ 1 len-so-far))))

;; Using a local function

(defun length12 (the-list)
  (labels
      ((length13 (list len-so-far)
         (if (null list)
             len-so-far
             (length13 (rest list) (+ 1 len-so-far)))))
    (length13 the-list 0)))

;; Other special forms

;; progn evaluates a sequence of forms and returns the last one
(progn (setf x 0) (setf x (+ x 1)) x)

;; return to break out of 'blocks'
(defun product (numbers)
  "Multiply all the numbers together to compute their product"
  (let ((prod 1))
    (dolist (n numbers prod)
      (if (= n 0)
          (RETURN 0)
          (setf prod (* n prod))))))

;; Macros

;; Try to define only when absolutely necessary and make the easy to understand

(defmacro while (test &rest body)
  "Repeat body while test is true"
  (list* 'loop
         (list 'unless test '(return nil))
         body))

;; Macro expand takes an example and shows you what the macro does

(macroexpand-1 '(while (< 1 10)
                 (print (* i i))
                 (setf i (+ i 1))))

(defmacro while (test &rest body)
  "Repeat body while test is true"
  (let ((code '(loop (unless test (return nil)) . body)))
    (subst test 'test (subst body 'body code))))

;; Backquote, comma and comma@
;; ` indicates mostly literal expression, with possiblity of evaluation
;; anything marked by , is evaluated and inserted into the structure
;; anything marked by ,@ must evaluate to a list that is spliced into the structure

(defmacro while (test &rest body)
  "Repeat body while test is true"
  `(loop (unless ,test (return nil))
         ,@body))

;; Examples
(setf test1 '(a test))

`(this is ,test1)
`(this is ,@test1)
`(this is . ,test1)
`(this is ,@test1 -- this is only ,@test1)

;; 3.3 Functions on Lists
(setq x '(a b c))
  (setq y '(1 2 3))

  ;; Key functions
  (first x)
  (second x)
  (third x)
  (nth 0 x)
  (rest x)
  (car x)
  (cdr x)
  (last x)
  (cdr x)
  (last x)
  (length x)
  (cons 0 y)
  (append x y)
  (list x y)
  (list* 1 2 x)
  (null nil)
  (null x)
  ;; T for any list including nil
  (listp x)
  (listp 3)
  (listp nil)
  ;;True for non-nil list
  (consp x)
  (consp nil)

  (equal x x)
  (equal x y)
  (sort y #'>)
  ;; Subsequence
  (subseq x 1 2)

  ;; Exercise 3.2
  ;; The function cons can be seen as a special case of one of the other functions listed, which one?
  ;; Special case of the list* function, where it takes only 1 item to insert and one list

  ;; Exercise 3.3
  ;; Write a function that will print an expression in dotted pair notation. Use built-in function princ to print each component of the expression

  (defun pr-rest (x)
    (princ " . ")
    (dprint x))

  (defun dprint (dp)
    "Prints the values of a dotted pair expression in dotted pair notation"
    (cond ((atom x) (princ x))
          (t (princ "(")
             (dprint (first x))
             (pr-rest (rest x))
             (princ ")")
             x)))

  ;; Exercise 3.4
  ;; Write a function that, like print, will print an expression in dotted pair notation when necessary but will use normal list notation when possible
  (defun pr-rest (x)
    (cond ((null x))
          ((atom x) (princ " . ") (princ x))
          (t (princ " ") (dprint (first x)) (pr-rest (rest x)))))

;; 3.6 Functions on Maintaining tables
(setf state-table '((AL . Alabama) (AK . Alaska) (AZ . Arizona) (AR . Arkansas)))

(assoc 'AK state-table)
(cdr (assoc 'AK state-table))
(assoc 'TX state-table)

;; Search by value rarther than key
(rassoc 'Arizona state-table)
(car (rassoc 'Arizona state-table))

;; Hash tables
(setf table (make-hash-table))

(setf (gethash 'AL table) 'Alabama)
(setf (gethash 'AK table) 'Alaska)
(setf (gethash 'AZ table) 'Arizona)
(setf (gethash 'AR table) 'Arkansas)

(gethash 'AK table)
(gethash 'TX table)

;; Remove key/value pairs with remhash
;; Clear all pairs with clrhash
;; Map over the key/value pair with maphash

;; Property lists
;; a-list: ((key1 . val1) (key2 . val2) ... (keyn . valn))
;; p-list: (key1 val1 key2 val2 ... keyn valn)

;; Property Tables

;; Building a property table using setf
(setf (get 'AL 'state) 'Alabama)
(setf (get 'AK 'state) 'Alaska)
(setf (get 'AZ 'state) 'Arizona)
(setf (get 'AR 'state) 'Arkansas)

(get 'AK 'state)
(get 'TX 'state)
;; No corresponding rassoc for property tables. A separate table would have to be created

(setf (symbol-plist 'state-table) '(AL Alabama AK Alaska AZ Arizona AR Arkansas))
(get 'state-table 'AL)
(get 'state-table 'Alaska)

;; 3.7 Functions on Trees
(setf tree '((a b) ((c)) (d e)))
  (tree-equal tree (copy-tree tree))

  (defun true (&rest ignore) t) ;; Ignores the leaves, which are the only things with 'values' in this example

  (defun same-shape-tree (a b)
    "Are two trees the same except for the leaves?"
    (tree-equal a b :test #'true))

  (same-shape-tree tree '((1 2) ((3)) (4 5)))
  (same-shape-tree tree '((1 2) (3) (4 5)))

  ;; Substituting expressions in a tree

  (subst 'new 'old '(old ((very old))))
  (sublis '((old . new)) '(old ((very old))))
  (subst 'new 'old 'old)

  (defun english->french (words)
    (sublis '((are . sommes) (book . libre) (house . maison) (friend . ami) (hello . bonjour) (bread . pain) (yellow . jaune) (you . tu) (my . mon) (your . ton) (eat . mange))
            words))

  (english->french '(hello my friend - eat your bread))

;; Functions on Numbers
(+ 4 2)
(- 4 2)
(* 4 2)
(/ 4 2)
(> 100 99)
(= 100 100)
(< 99 100)
(random 100)
(expt 4 2)
(sin pi)
(asin 0)
(min 2 3 4)
(abs -3)
(sqrt 4)
(round 4.1)
(rem 11 5)
(mod 11 5)

;; 3.9 Functions on Sets
;; Use of a list to represent a set

  (setf r '(a b c d))
  (setf s '(c d e))

  ;; Useful functions
  (intersection r s) ;; Common elements
  (union r s) ;; Collection of all elements
  (set-difference r s) ;; Whatever isn't in the intersecton belonging the sets
  (member 'd r) ;; Check if element is a member of the set
  (subsetp s r) ;; Returns true if s is a subset of r
  (adjoin 'b s) ;; Adds to the set if not already present
  (adjoin 'c s)

;; 3.10 Destructive Functions 

(setf x '(a b c))
(setf y '(1 2 3))
(append x y)

;; Function that changes the value of the inputs
(nconc x y)
x
y

;; Exercise 3.5
;; Write a program that will play the role of the guesser in the game Twenty Questions. The user of the program will have in mind any type of thing. The program will ask questions of the user, which must be answered yes or no, or "it" when the program has guessed it. If the program runs out of guesses, it gives up and asks the user what "it" was. At first the program will not play well, but each time it plays, it will remember the user's replies and use them for subsequent guesses.

(defstruct pokemon
  name
  primary-type
  (secondary-type nil)
  (stage 'basic)
  pokedex-no.)

;; List of first-generation pokemon
(setq pokedex
      '((bulbasaur ivysaur venusaur)
        (charmander charmeleon charizard)
        (squirtle wortortle blastoise)
        (caterpie metapod butterfree)
        (weedle kakuna beedrill)
        (pidgey pidgeotto pidgeot)
        (rattata raticate)
        (spearow fearow)
        (ekans arbok)
        (pikachu raichu)
        (sandshrew sandslash)
        (nidoran-f nidorina nidoqueen)
        (nidoran-m nidorino nidoking)
        (clefairy clefable)
        (vulpix ninetales)
        (jigglypuff wigglytuff)
        (zubat golbat)
        (oddish gloom vileplume)
        (paras parasect)
        (venonat venomoth)
        (diglett dugtrio)
        (meowth persian)
        (psyduck golduck)
        (mankey primeape)
        (poliwag poliwhirl poliwrath)
        (abra kadabra alakazam)
        (machop machoke machamp)
        (bellsprout weepinbell victreebel)
        (tentacool tentacruel)
        (geodude graveler golem)
        (ponyta rapidash)
        (slowpoke slowbro)
        (magnemite magneton)
        (farfetch'd)
        (doduo dodrio)
        (seel dewgong)
        (grimer muk)
        (shellder cloyster)
        (ghastly haunter gengar)
        (onix)
        (drowzee hypno)
        (krabby kingler)
        (voltorb electrode)
        (exeggcute exeggutor)
        (cubone marowak)
        (hitmonlee)
        (hitmonchan)
        (lickitung)
        (koffing weezing)
        (rhyhorn rhydon)
        (chansey)
        (tangela)
        (kangaskhan)
        (horsea seadra)
        (goldeen seaking)
        (staryu starmie)
        (mr.mime)
        (scyther)
        (jynx)
        (electabuzz)
        (magmar)
        (pinsir)
        (tauros)
        (magikarp gyarados)
        (lapras)
        (ditto)
        (eevee vaporeon jolteon flareon)
        (porygon)
        (omanyte omastar)
        (kabuto kabutops)
        (snorlax)
        (articuno)
        (zapdos)
        (moltres)
        (dragonair dragonite)
        (mewtwo)
        (mew)))



(setq bulbasaur (make-pokemon :name 'bulbasaur :primary-type 'electric :pokedex-no. 1))



(defvar *pokedex*
  '((make)
    )
  )

  (defstruct type )

(setq colours '(red yellow orange green blue violet black white brown))

(setq shape '(circle triangle square box ball))

(setq size '(tiny small medium large enormous))

(setq clues
      '((make-guess :type 'Vegetable) ))


(defun associate (type clue)
  )

;; 3.12 Input/Output

;; Create file test.txt and write to it

(with-open-file (stream "test.txt" :direction :output)
  (print '(hello there) stream)
  (princ 'goodbye stream))

;; Read from the same file
(with-open-file (stream "test.txt" :direction :input)
  (list (read stream) (read-char stream) (read stream)
        (read stream nil 'eof)))

;; terpri stands for "terminate print line" and it skips to the next line
;; fresh-line does the same unless it determines it is already at the front of a line

(format t "hello world")

(format t "~&~a plus ~s is ~f" "two" "two" 4)
;; ~& moves to a fresh line
;; ~a prints the next argument as princ would
;; ~s prints the next argument as prin1 would
;; ~f prints a number in floating point format

(let ((numbers '(1 2 3 4 5)))
  (format t "~&~{~r~^ plus ~} is ~@r"
          numbers (apply #'+ numbers)))
;; ~r prints the next argument, which should be a number, in english
;; ~@r prints the next argument, which should be a number, in roman numerals
;; ~{...~} takes the next argument, which should be a list and formats each of the contents according to what is between the braces
;; ~^ exits from the enclosing loop if there are no more arguments remaining

;; 3.13 Debugging Tools
;; We have already seen trace and untrace

;; step can be used to halt execution before each subform is evaluated
;; in LispWorks
;; (step (+ 3 4 (* 5 6 (/ 7 8))))


;; apropos prints information about all symbols which match the argument

(apropos 'string)

;; describe gives more informatino on a specific object

(describe 'make-string)

(describe 1234.56)

;; documentation gives a documentation string
(documentation 'first 'function)
(documentation 'pi 'variable)

;; inspect

;; 3.14 Antidebugging Tools

;; error and cerror are used to signal an error condition


;; error takes a format sting and optional arguments
;; It stops the program upon being activated
(defun average (numbers)
  (if (null numbers)
      (error "Average of the empty list is undefined.")
      (/ (reduce #'+ numbers)
         (length numbers))))

;; cerror stands for continuable erro
;; cerror takes two format strings
;; The first prints what happens if we continue
;; The second prints the actual error
;; the user can continue by typing :continue

(defun average (numbers)
  (if (null numbers)
      (progn
        (cerror "Use 0 as the average."
                "Average of the empty list is undefined.")
        0)
      (/ (reduce #'+ numbers)
         (length numbers))))

;; ecase for "exhaustive case" or "error case" will generate an error message if none of the cases are satisfied

;; check-type raises an error upon receiving the wrong type
(defun sqr (x)
  "Multiply x by itself"
  (check-type x number)
  (* x x))

;; assert raises an error if the value passed to it is false
;; the user will be given the opportunity to assign a value to x
;; assert always returns nil
(defun sqr (x)
  "Multiply x by itself"
  (assert (numberp x))
  (* x x))

(defun eat-porridge (bear)
  (assert (< too-cold (temperature (bear-porridge bear)) too-hot)
          (bear (bear-porridge bear))
          "~a's porridge is just right: ~a"
          bear (hotness (bear-porridge bear))))

(eat-porridge mama-bear)

;; Timing Tools

;; time calculates how long a function takes to execute the expression given
(defun f (n)  (dotimes (i n) nil))

(time (f 10000))

;; Compiles the function instead of just interpreting it
(compile 'f)

(time (f 10000))

;; 3.15 Evaluation

(+ 1 2 3 4)

;; funcall is used to apply a function to an individual arguments
(funcall #'+ 1 2 3 4)

;; apply is used to apply a function to a list of arguments. IT can actually be given many arguments before the finargument which must be a list
(apply #'+ '(1 2 3 4))
(apply #'+ 1 2 '(3 4))

;; eval takes a single argument and evaluates it as would the repl
(eval '(+ 1 2 3 4))

;; 3.16 Closures

(mapcar #'(lambda (x) (+ x x)) '(1 3 10))

;; Lexical closure. Variables are closed withing the function

(defun adder (c)
  "Return a function that adds c to its argument"
  #'(lambda (x) (+ x c)))

(mapcar (adder 3) '(1 3 10))

(mapcar (adder 10) '(1 3 10))

;;  bank-account returns a closure that can be used as a representation of a bank account
(defun bank-account (balance)
  "Open a bank account starting with the given balance"
  #'(lambda (action amount)
      (case action
        (deposit (setf balance (+ balance amount)))
        (withdraw (setf balance (- balance amount))))))

;; Now multiple bank accounts can be made with different closures, each with a separate value for the lexical variable balance

(setf my-account (bank-account 200.00))

(setf your-account (bank-account 500.00))

(funcall my-account 'withdraw 75.00)

(funcall your-account 'deposit 250.00)

(funcall your-account 'deposit 100.00)

(funcall my-account 'withdraw 25.00)

;; 3.17 Special Variables

;; Lexical variables vs special variables

;; By defualt Common Lisp variables are lexical variables
;; Introduces with let, defun, etc...

;; Variables are made special with the use of defvar of defparameter

(defvar *counter* 0)
;; This variable can be referred to anywhere in the program
;; Special variables can be bound both globally and locally

(defun report1 ()
  (format t "Counter = ~d" *counter*))

(report1) ;; => 0
(let ((*counter* 100)) 
  (report1)) ;; => 100
(report1) ;; => 0

;; In these two examples, var, as special variable is being set in an equivalent manner
(setf (symbol-value 'var) value)
(set var value)

;; Exercise 3.6
;; Given the following initialization for the lexical variable a and the special variable *b*, what will be the value of the let form?
(setf a 'global-a) ;; Lexical binding
(defvar *b* 'global-b) ;; Special variable

(defun fn () *b*)

(let ((a 'local-a)
      (*b* 'local-b))
  (list a *b* (fn) (symbol-value 'a) (symbol-value '*b*)))

;; => (local-a local-b local-b global-a global-b)

;; Multiple Values

(round 5.1) ;; Returns both the rounded integer and the remaining fraction

;; Take the output of second argument and bind them to the list of symbols provided
(defun show-both (x)
  (multiple-value-bind (int rem)
      (round x)
    (format t "~f = ~d + ~f" x int rem)))

(show-both 5.1)

(values 1 2 3)

;; 'values' can be used to return no values at all, such as in describe
(describe 'x)

(list (describe 'x))

;; 3.21 Exercises

;; Exercise 3.9
;; Write a version of length using the function reduce

(defun length-reduce (xs)
  (reduce #'(lambda (x xs) (+ x 1)) xs :initial-value 0))

(length-reduce '(100 2 3 4 5 23 2 32 23))

;; Exercise 3.10
;; Use a reference manual or 'describe to figure out what the fucntions lcm and nreconc do

(describe #'lcm)

;; lcm returns the least common multiple of a list of numbers
(lcm 8 64 12 6)

(describe #'nreconc)
(describe #'nreverse) ;; return a sequence of the same elements in revers order
(describe #'nconc) ;;  concateneates the liss given as arguments

(nreconc '(1 2 3) '(1 2)) ;; Takes two lists, reverses the first one and concatenatesa them

;; Exercise 3.11
;; There is a built-in Common Lisp function that, given a key, a value, and an association list, returns a new association list that is extended to include the key/value pair. What is the name of the function?

;;acons
(setf a-list '((cat . meow) (dog . woof)))

(assoc 'cat a-list)

(acons 'cow 'moo a-list)

;; 3.12 Exercises
;; Write a single expression using format that will take a list of words and print them as a sentence, with the first word capitalized and a period after the last word. Consult a reference to learn new format directives.

(defun specific-format (xs)
  (format t "~@(~{~a~^ ~}~)." xs))

(specific-format '(cat dog horse cow))
