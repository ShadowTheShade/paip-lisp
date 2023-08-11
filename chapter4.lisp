;; Chapter 4
   ;; 4.2 Stage 2 : Specification
   ;;(GPS '(unknown poor) '(rich famous) list-of-ops)

   ;; Function used in GPS
   (defun find-all (item sequence
               &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
"Find all the elements of a sequence that match the item, according to the keywords without altering the sequence"
(if test-not
    (apply #'remove item sequence
           :test-not (complement test-not) keyword-args)
    (apply #'remove item sequence
           :test (complement test) keyword-args)))

;; 4.3 Stage 3 : Implementation
(defvar *state* nil "The current state: a list of conditions.")

(defvar *ops* nil "A list of available operators.")

(defstruct op "An operation"
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun GPS (*state* goals *ops*)
  "General Problem Solver: achieve all goals using *ops*."
  (if (every #'achieve goals) 'solved))

(defun achieve (goal)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (or (member goal *state*)
    (some #'apply-op
      (find-all goal *ops* :test #'appropriate-p))))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member goal (op-add-list op)))

(defun apply-op (op)
  "Print a message and update *state* if op is applicable."
  (when (every #'achieve (op-preconds op))
    (print (list 'executing (op-action op)))
    (setf *state* (set-difference *state* (op-del-list op)))
    (setf *state* (union *state* (op-add-list op)))
  t))


;; Special variables that can be accessed from anywhere within the program

      (defvar *state* nil "The current state: a list of conditions.")
      (defvar *ops* nil "A list of available operators.")

      (defstruct op
        "An operation"
        (action nil)
        (preconds nil)
        (add-list nil)
        (del-list nil))

      ;; Defining a structure automatically defines a constructor called make-op
      ;; It also creates accessor functions op-action, op-preconds, op-add-list and op-del-list
      ;; It defines a copier copy-op, a predicate op-p and setf definitions for changing each slot

      ;;(defun make-op (&key action preconds add-list del-list)
      ;;  (vector 'op action preconds add-list del-list))

      ;; (defun op-action (op) (elt op 1))
      ;; (defun op-preconds (op) (elt op 2))
      ;; (defun op-add-list (op) (elt op 3))
      ;; (defun op-del-list (op) (elt op 4))

      ;; (defun copy-op (op (copy-seq op)))

      ;; (defun op-p (op)
      ;;   (and (vectorp op) (eq (elt op 0) 'op)))

;; Stage 4 : Test

(make-op :action 'drive-son-to-school
         :preconds '(son-at-home car-works)
         :add-list '(son-at-school)
         :del-list '(son-at-home))

(defparameter *school-ops*
  (list
    (make-op :action 'drive-son-to-school
             :preconds '(son-at-home car-works)
             :add-list '(son-at-school)
             :del-list '(son-at-home))
    (make-op :action 'shop-installs-battery
             :preconds '(car-needs-battery shop-knows-problem shop-has-money)
             :add-list '(car-works))
    (make-op :action 'tell-shop-problem
             :preconds '(in-communication-with-shop)
             :add-list '(shop-knows-problem))
    (make-op :action 'telephone-shop
             :preconds '(know-phone-number)
             :add-list '(in-communication-with-shop))
    (make-op :action 'look-up-number
             :preconds '(have-phone-book)
             :add-list '(know-phone-number))
    (make-op :action 'give-shop-money
             :preconds '(have-money)
             :add-list '(shop-has-money)
             :del-list '(have-money))))

(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school)
     *school-ops*) ;; => Solved

(gps '(son-at-home car-needs-battery have-money)
     '(son-at-school)
     *school-ops*) ;; => nil

(gps '(son-at-home car-works)
     '(son-at-school)
     *school-ops*) ;; => Solved

;; 4.7 The Clobbered Sibling Problem
;; The following situation should now have no solution, since we need to keep extra money for the day/
  (gps '(son-at-home car-needs-battery have-money have-phone-book)
       '(have-money son-at-school)
       *school-ops*)

;; Ensure achieve all asks for all conditions to be met at the same time
;; Returns true if we can acheive each goal and if the goal is a subset of the final state
(defun achieve-all (goals)
  "Try to achieve each goal all together"
  (and (every #'achieve goals) (subsetp goals *state*)))

(defun GPS (*state* goals *ops*)
  "General Problem Solver: achieve all goals using *ops*."
  (if (achieve-all goals) 'solved))

;; 4.8 The Leaping Before You Look Problem
(gps '(son-at-home car-needs-battery have-money have-phone-book)
    '(son-at-school have-money)
    *school-ops*)

;; 4.9 The Recursive Subgoal Problem
;; Add operation that requires you to be in communication with someone from the shop in order to get a phone number
(push (make-op :action 'ask-phone-number
               :preconds '(in-communication-with-shop)
               :add-list '(know-phone-number))
      *school-ops*)

;; This raises an error since the recursion depth is too large. 
(gps '(son-at-home car-needs-battery have-money)
     '(son-at-school)
     *school-ops*)

;; Tracing acheive may give us insight into the problem
(trace achieve)

; 4.10 The Lack of Intermediate Information Problem

;; Sample format for dbg :identifier -> message 
(dbg :gps "The current goal is: ~a" goal)

;; If debugging is turned on for the identifier :gps, then any calls to dbg with :gps will print output
(debug :gps)
(undebug :gps) ;; Turns off debugging

;; *debug-io* is the stream normally used for debugging input / output

;; fresh-line advances to the next line of output, unless it is already there

(defvar *ddb-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun debug (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
 "Stop dbg on the ids. With no ids, stop dbg altogether."
 (setf *dbg-ids* (if (null ids) nil
                     (set-difference *dbg-ids* ids))))

;; prints debug information with indentation

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ " " *debug-io*))
    (apply #'format *debug-io* format-string args)))
