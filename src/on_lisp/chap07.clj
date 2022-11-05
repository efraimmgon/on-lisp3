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

;;; ----------------------------------------------------------------------------
;;; 7.10 Macros from Functions

;; The difficulty of translating a function into a macro depends on a number 
;; of properties of the function. The easiest class to translate are the functions 
;; which

;; 1. Have a body consisting of a single expression.
;; 2. Have a parameter list consisting only of parameter names.
;; 3. Create no new variables (except the parameters).
;; 4. Are not recursive (not part of a mutually recursive group).
;; 5. Have no parameter which occurs more than once in the body.
;; 6. Have no parameter whose value is used before that of another parameter 
;; occurring before it in the parameter list.
;; 7. Contain no free variables.

;; Where a function definition meets all the conditions above, you can easily 
;; transform it into an equivalent macro definition. Simply put a backquote in 
;; front of the body and a comma in front of each symbol which occurs in the 
;; parameter list:
(defmacro our-second [x] `(first (rest ~x)))

;; The technique changes slightly when the body has more than one expression, 
;; because a macro must expand into a single expression. So if condition 1 
;; does't hold, you have to add a progn.
(defmacro noisy-second [x]
  `(do (println "Someone is taking a cadr!")
       (first (rest ~x))))

;; When the function doesn't meet condition 2 because it has an &rest or 
;; &body parameter, the rules are the same, except that the parameter, 
;; instead of simply having a comma before it, must be spliced into a call 
;; to list.

(defn sum [& args]
  (apply + args))

(defmacro sum [& args]
  `(+ ~@args))

;; When condition 3 doesn't hold - when new variables are created within the 
;; function body - the rule about the insertion of commas must be modified. 
;; Instead of putting commas before all symbols in the parameter list, we only 
;; put them before those which will refer to the parameters.

(defn foo [x y z]
  (list x (let [x y]
            (list x z))))

(defmacro foo [x y z]
  `(list ~x (let [x# ~y]
              (list x# ~z))))

