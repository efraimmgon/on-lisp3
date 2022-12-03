(ns on-lisp.chap18
  (:require
   [on-lisp.utils :as utils :refer [acond aif]]))

;;; ----------------------------------------------------------------------------
;;; 18
;;; Destructuring
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; 18.4 Matching

(defn varsym?
  "Returns true if `x` is a variable in the context of `match`. Variables 
   are symbols staring with the `?` char."
  [x]
  (and (symbol? x)
       (= (first (name x))
          \?)))

#_(varsym? 'x)
#_(varsym? '?x)

(defn binding [x binds]
  (when-let [val (get binds x)]
    (or (binding val binds)
        val)))

; - Like Prolog, match treats _ (underscore) as a wild-card. It matches 
; everything, and has no effect on the bindings.
; - Compares its arguments element by element, building up assignment of 
; values to variables, called bindings, in the paremeter binds. If the match  
; is successful, returns the bindings generated, otherwise returns nil.
(defn match
  ([x y]
   (match x y {}))
  ([x y binds]
   (acond
    (or (= x y) (= x '_) (= y '_))
    binds

    (binding x binds)
    (match it y binds)

    (varsym? x) (assoc binds x y)

    (varsym? y) (assoc binds y x)

    (and (coll? x) (coll? y)
         (match (first x) (first y) binds))
    (match (next x) (next y) it)

    :else nil)))

#_(match '(p a b c a) '(p ?x ?y c ?x))
#_(match '(a b c) '(a a a))
#_(match '(a ?x b) '(_ 1 _))