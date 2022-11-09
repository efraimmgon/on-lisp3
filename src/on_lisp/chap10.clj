(ns on-lisp.chap10
  (:require
   [on-lisp.utils :as utils]))

;;; ----------------------------------------------------------------------------
;;; 10
;;; Other Macro Pitfalls
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; 10.1 Number of Evaluations

;; A correct version:
(defmacro our-for [[var start stop] & body]
  `(loop [~var ~start
          stop# ~stop]
     (when-not (> ~var stop#)
       (recur (inc ~var) stop#))))

;; Subject to multiple evaluations:
#_(defmacro our-for [[var start stop] & body]
    `(loop [~var ~start]
       (when-not (> ~var ~stop)
         (recur (inc ~var)))))

;; Incorrect order of evaluation:
#_(defmacro our-for [[var start stop] & body]
    `(loop [stop# ~stop
            ~var ~start]
       (when-not (> ~var stop#)
         (recur (inc ~var) stop#))))


;;; ----------------------------------------------------------------------------
;;; 10.4 Recursion

#_(defn ntha [n lst]
    (if (zero? n)
      (first lst)
      (ntha (dec n) (next lst))))

(comment
  "Superficially, nthb appears to be equivalent to ntha, but a program 
   containing a call to nthb would not compile, because the expansion of the 
   call would never terminate."

  "The trouble with nthb is that every expansion contains a reference to nthb 
   itself. The function version, ntha, terminates because it recurses on the 
   value of n, which is decremented on each recursion. but macroexpansion only 
   has access to forms, not to their values."

  "It's fine for a macro to expand into a call to itself, just so long as it
   doesn't always do so")
#_(defmacro nthb [n lst]
    `(if (zero? ~n)
       (first ~lst)
       (nthb (dec ~n) (next ~lst))))

(comment
  "A tail-recursive function can easily be transformed into an iterative 
   equivalent, and then used as a model for a macro.")

(defmacro nthc [n lst]
  `(loop [n# ~n
          lst# ~lst]
     (if (zero? n#)
       (first lst#)
       (recur (dec n#) (next lst#)))))

(comment
  "Depending on what you need a macro for, you may find it sufficient to use 
   instead a combination of macro and function. `nthd` is simply to make the 
   macro expand into a call to a recursive function. If for example, you need 
   a macro only to save users the trouble of quoting arguments, then this 
   approach should suffice.")

(defn nth-fn [n lst]
  (if (zero? n)
    (first lst)
    (recur (dec n) (rest lst))))

(defmacro nthd [n lst]
  `(nth-fn ~n ~lst))

(comment
  "If you need a mcro because you want its whole expansion to be inserted into 
   the lexical environment of the macro call, then you would more likely want 
   to follow the example of nthe. Each expansion of nthe will have its own 
   version of such a function within it.")

(defmacro nthe [n lst]
  `(letfn [(nth-fn [n lst]
             (if (zero? n)
               (first lst)
               (recur (dec n) (rest lst))))]
     (nth-fn ~n ~lst)))

(comment
  "Although orb recurses, it recurses on the arguments to the macro (which
   are available at macroexpansion time), not upon their values 
   (which aren't).")

(defn or-expand [args]
  (when (seq args)
    `(let [sym# ~(first args)]
       (if sym#
         sym#
         ~(or-expand (next args))))))

(defmacro ora [& args]
  (or-expand args))

(defmacro orb [& args]
  (when (seq args)
    `(let [sym# ~(first args)]
       (if sym#
         sym#
         (orb ~@(next args))))))