(ns on-lisp.chap15
  (:require
   [on-lisp.utils :as utils]))

;;; ----------------------------------------------------------------------------
;;; 15
;;; Macros Returning Functions
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; 15.1 Building Functions

(declare rbuild)

(defn build-compose [fns]
  (let [g (gensym)]
    `(fn [~g]
       ~(letfn [(rec [fns]
                  (if (seq fns)
                    `(~(rbuild (first fns))
                      ~(rec (next fns)))
                    g))]
          (rec fns)))))

(defn build-call [op fns]
  (let [g (gensym)]
    `(fn [~g]
       (~op ~@(map (fn [f]
                     `(~(rbuild f) ~g))
                   fns)))))

(defn rbuild [expr]
  (if (or (utils/atom? expr)
          (= (first expr) 'fn))
    expr
    (if (= (first expr) 'comp)
      (build-compose (next expr))
      (build-call (first expr) (next expr)))))

; Note: The original case for compose makes no sense for clj because it's a 
; lisp-2.
(defmacro fn>
  "Builds compound functions from their descriptions. Its argument should be 
   an expression of the form (operator . arguments). The operator can be the 
   name of a function or macro - *or compose, which is treated specially. The 
   arguments can be names of functions or macros of one argument, or 
   expressions that could be arguments to fn*.
   
   For instance, 
   (fn* (and integer? odd?)) 
   yields a function equivalent to
   (fn [x] (and (integer? x) (odd? x)))
   
   If we use compose as the operator, we get a function representing the 
   composition of the arguments, but without the explict calls that were 
   needed when compose was defined as a function. For example,
   (fn* (compose list inc Math/round))
   expands into:
   (fn [x] (list (inc (Math/round x))))
   which enables inline compilation of simple functions like list and inc."
  [expr]
  (rbuild expr))

#_(utils/mac (fn> (and integer? odd?))) ; default case
#_(utils/mac (fn> (comp list inc Math/round))) ; compose
#_(utils/mac (fn> (comp (fn [x] (+ x 3)) Math/round))) ; anonymous fn

; intersection
#_(filter (fn> (and integer? odd?))
          '[c 3 p 0])

; union
#_(filter (fn> (or integer? symbol?))
          '[c 3 p 0.2])

; conditional
#_(map (fn> (if odd? inc identity))
       (range 1 7))

; creating a sequence
#_(map (fn> (list dec identity inc))
       [1 2 3])

; manipulating a sequence
#_(remove (fn> (or (and integer? odd?)
                   (and coll? next)))
          '[1 [a b] c [d] 2 3.4 [e f g]])

;;; ----------------------------------------------------------------------------
;;; 15.2 Recursion on Cdrs

; don't really care about recursion on cdrs - it makes the code harder to 
; understand.

;;; ----------------------------------------------------------------------------
;;; 15.3 Recursion on Subtrees

; same as above

;;; ----------------------------------------------------------------------------
;;; 15.4 Lazy Evaluation

(comment
  "A delay is represented as a two-part structure. The first field indicates 
   whether the delay has been evaluated yet, and if it has, contains the value. 
   The second field contains a closure which can be called to find the value 
   that the delay represents.")

(def unforced
  "A gensym representing a Delay that hasn't been forced yet."
  (gensym))

(defrecord Delay [forced closure delay?])

(defn make-delay \
  "Takes a map and returns a Delay wrapped in an Atom."
  [{:keys [forced closure]}]
  (atom (->Delay forced closure ::delay)))

(defn delay-p
  "Returns true if x is a Delay."
  [x]
  (and (instance? clojure.lang.Atom x)
       (map? @x)
       (= ::delay (:delay? @x))))

(defmacro delay*
  "Takes an expression and returns a delay representing its value."
  [expr]
  `(let [self# (make-delay {:forced unforced})]
     (swap! self# assoc :closure
            (fn []
              (let [expr# ~expr]
                (swap! self# assoc :forced expr#)
                expr#)))
     self#))

(defn force*
  "For ordinary objects it is the identity function, but for delays it is a 
   demand for the value that the delay represents."
  [x]
  (if (delay-p x)
    (if (= (:forced @x) unforced)
      ((:closure @x))
      (:forced @x))
    x))

#_(let [x 2]
    (def d (delay* (inc x))))
#_(force* d)
