(ns on-lisp.chap07
  (:require
   clojure.pprint
   [on-lisp.utils :as utils]))

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

;;; ----------------------------------------------------------------------------
;;; 7.6 A Model of Macros

(defmacro our-expander [name]
  `(:expander (meta #'~name)))

(defmacro our-defmacro [name params & body]
  (let [g (gensym)]
    `(do
       (alter-meta! #'~name
                    assoc :expander
                    (fn [~g]
                      (let [~params (rest ~g)]
                        ~@body))))))

; Note: this throws for some reason
#_(defn our-macroexpand-1
    [expr]
    (if (and (utils/consp expr)
             (our-expander (first expr)))
      ((our-expander (first expr)) expr)
      expr))

;;; ----------------------------------------------------------------------------
;;; 7.7 Macros as Programs

#_(cl-do [[w 3]
          [x 1 (inc x)]
          [y 2 (inc y)]
          [z]]
         [(> x 10) (prn z) y]
         (prn x)
         (prn y))

; should expand into something like

#_(loop [w 3
         x 1
         y 2
         z nil]
    (if (> x 10)
      (do (prn z)
          y)
      (do (prn x)
          (prn y)
          (recur w (inc x) (inc y) z))))

;;; ----------------------------------------------------------------------------
;;; 7.8 Macro Style

(defmacro our-and [& args]
  (case (count args)
    0 true
    1 (first args)
    `(if (first args)
       (our-and ~@(rest args)))))

