(ns on-lisp.chap14
  (:require
   [on-lisp.utils :as utils]))

;;; ----------------------------------------------------------------------------
;;; 14
;;; Anaphoric Macros
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; 14.1 Anaphoric Variants

(defmacro aif
  "Anaphoric if. `it` is used to bind the result of `test-form`."
  [test-form then-form & [else-form]]
  `(let [~'it ~test-form]
     (if ~'it ~then-form ~else-form)))

#_(utils/mac (aif (inc 1) it false))

(defmacro awhen
  "Anaphoric when. `it` is used to bind the result of `test-form`."
  [test-form & body]
  `(let [~'it ~test-form]
     (when ~'it
       ~@body)))

(defmacro awhile
  "Anaphoric while `it` is used to bind the result of `test-form`."
  [test-form & body]
  `(loop [~'it ~test-form]
     (when ~'it
       ~@body
       (recur ~test-form))))

(defmacro aand
  "Anaphoric and. During the evaluation of each of its arguments, `it` will 
   be bound to the value returned by the previous argument."
  [& args]
  (cond (empty? args) true
        (empty? (next args)) (first args)
        :else `(aif ~(first args)
                    (aand ~@(next args)))))

; In practice, `aand` tends to be used in programs which make conditional 
; queries, as in:
; (aand (owner x) (address it) (town it))

(defmacro acond
  "Anaphoric cond. Meant for cases where the remainder of a cond clause wants 
   to use the value returned by the expression."
  [& clauses]
  (when (seq clauses)
    (let [cl1 (take 2 clauses)]
      `(let [sym# ~(first cl1)]
         (if sym#
           (let [~'it sym#]
             ~(last cl1))
           (acond ~@(drop 2 clauses)))))))

#_(utils/mac (acond (odd? 2) (prn it)
                    (+ 1 1) (prn it)))

; Note: not much use in clj since we can name anonymous fns.
(defmacro alambda
  "Anaphoric lambda; (uses `self`) for referring literally to recursive 
   functions."
  [params & body]
  `(letfn [(~'self ~params ~@body)]
     ~'self))

#_(utils/mac (alambda [x] (if (zero? x) 1 (* x (self (dec x))))))

; don't know how to implement ablock

#_(ablock :north-pole
          (utils/princ "ho ")
          (utils/princ it)
          (utils/princ it)
          (:return-from :north-pole))