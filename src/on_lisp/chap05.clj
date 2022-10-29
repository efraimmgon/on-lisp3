(ns on-lisp.chap05
  (:require
   [on-lisp.utils :as utils]))

;;; ----------------------------------------------------------------------------
;;; 5
;;; Returning functions
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Composing functions

(defn fif
  "Takes 3 functions, one for the test clause, one for the then, 
   one for the else."
  ([test then]
   (fif test then nil))
  ([test then else]
   (fn [x]
     (if (test x)
       (then x)
       (when else
         (else x))))))

#_(def people
    [{:slave true
      :owner :state}
     {:slave false
      :employer :google}])
#_(map (fn [x]
         (if (:slave x) (:owner x) (:employer x)))
       people)
#_(map (fif :slave :owner :employer) people)

(defn fint
  "Function intersection. Takes an arbitrary number of predicates and returns
   a function that returns true if all the predicates return true."
  [f & fns]
  (if (seq fns)
    (let [chain (apply fint fns)]
      (fn [x]
        (and (f x)
             (chain x))))
    f))

#_(def docs
    [{:id 1
      :signed true
      :sealed true
      :delivered true}
     {:id 2
      :signed true
      :sealed false
      :delivered true}])
#_(filter (fn [x]
            (and (:signed x) (:sealed x) (:delivered x)))
          docs)
#_(filter (fint :signed :sealed :delivered)
          docs)

(defn fun
  "Function union. Takes an arbitrary number of predicates and returns 
   a function that returns true if any of the predicates return true."
  [f & fns]
  (if (seq fns)
    (let [chain (apply fun fns)]
      (fn [x]
        (or (f x)
            (chain x))))
    f))

#_(filter (fun :signed :sealed :delivered)
          docs)

;;; ----------------------------------------------------------------------------
;;; Recursion on Cdrs

(defn lrec
  "List recursion. Takes a function of two arguments: the current car of 
   the list, and a function which can be called to continue the recursion.
   The second optional argument is the base fn or value to be accumulated."
  ([rec]
   (lrec rec nil))
  ([rec base]
   (letfn [(self [lst]
             (if (seq lst)
               (rec (first lst)
                    #(self (rest lst)))
               (if (fn? base)
                 (base)
                 base)))]
     self)))

;; count
#_((lrec (fn [x f] (inc (f)))
         0)
   [:a :b :c])

;; our-every
#_((lrec (fn [x f] (and (odd? x) (f)))
         true)
   [1 3 5 7])

;; remove-duplicates
#_((lrec (fn [x f] (conj (f) x))
         #{})
   [1 1 2 2 3 3])

;;; ----------------------------------------------------------------------------
;;; Recursion on Subtrees

(defn count-leaves [tree]
  (if (utils/atom? tree)
    1
    (+ (count-leaves (first tree))
       (or (when (next tree)
             (count-leaves (rest tree)))
           1))))

#_(count-leaves '[[a b [c d]] [e] f])

(defn rfind-if [f tree]
  (if (utils/atom? tree)
    (and (f tree) tree)
    (or (rfind-if f (first tree))
        (if (next tree)
          (rfind-if f (next tree))))))

#_(rfind-if (fint number? odd?) [2 [3 4] 5])

(defn ttrav
  "Archetypal function for recursion on subtrees. The `rec` function takes two
    args, one for the left subtree and one for the right. If the `base` is 
   a function it will be called on the current leaf."
  ([rec]
   (ttrav rec identity))
  ([rec base]
   (letfn [(self [tree]
             (if (utils/atom? tree)
               (if (fn? base)
                 (base tree)
                 base)
               (rec (self (first tree))
                    (when-let [more (next tree)]
                      (self more)))))]
     self)))

;; copy-tree
#_(ttrav cons identity)

;; count-leaves
#_(ttrav (fn [l r]
           (+ l (or r 1)))
         1)

;; flatten
#_(ttrav concat mklist)

(defn trec
  "Tree recurser. The second arg to trec should be a function of three 
   arguments: the current object and the two recursers. The latter two will 
   be closures representing the recursions down the left and right subtrees."
  ([rec]
   (trec rec identity))
  ([rec base]
   (letfn [(self [tree]
             (if (utils/atom? tree)
               (if (fn? base)
                 (base tree)
                 base)
               (rec tree
                    #(self (first tree))
                    #(when-let [more (next tree)]
                       (self more)))))]
     self)))

;; flatten
#_(trec (fn [o l r]
          (concat (l) (r)))
        mklist)

;; rfind-if, for odd
#_(trec (fn [o l r]
          (or (l) (r)))
        (fn [tree]
          (and (odd? tree) tree)))