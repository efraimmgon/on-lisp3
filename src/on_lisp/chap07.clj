(ns on-lisp.chap07
  (:require
   clojure.pprint))

;;; ----------------------------------------------------------------------------
;;; 7
;;; Macros
;;; ----------------------------------------------------------------------------

(defmacro nif
  "A three-way numeric if. The first argument should evaluate to a number. 
   Then the second, third, or fourth argument is evaluated, depending on 
   whether the first was positive, zero, or negative."
  [expr pos zero neg]
  `(let [expr# ~expr]
     (cond
       (pos? expr#) ~pos
       (zero? expr#) ~zero
       :else ~neg)))

#_(map #(nif % :p :z :n)
       [0 2.5 -8])

(defmacro our-when
  [test & body]
  `(if ~test
     (do ~@body)))

;;; ----------------------------------------------------------------------------
;;; 7.4 Testing Macroexpansion

(defmacro mac
  "Pretty printing for macroexpand-1."
  [expr]
  `(clojure.pprint/pprint
    (macroexpand-1 '~expr)))

(defmacro when-bind [[var expr] & body]
  `(let [~var ~expr]
     (when ~var
       ~@body)))

#_(when-bind
   [lst (range 5)]
   (map inc lst))

#_(mac (when-bind [x (range 5)]
                  (prn x)))

