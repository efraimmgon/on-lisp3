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

#_(build-compose '(list inc Math/round))

(defn build-compose [fns]
  (let [g (gensym)]
    `(fn [~g]
       ~(letfn [(rec [fns]
                  (if (seq fns)
                    `(~(rbuild (first fns))
                      ~(rec (next fns)))
                    g))]
          (rec fns)))))

#_(build-call 'and '(integer? odd?))

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
