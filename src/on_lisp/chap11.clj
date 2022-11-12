(ns on-lisp.chap11
  (:require
   [on-lisp.utils :as utils]))

;;; ----------------------------------------------------------------------------
;;; 11
;;; Classic Macros
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; 11.1 Creating Context

(defmacro our-let [binds & body]
  `((fn ~(mapv #(first %) (partition 2 binds))
      ~@body)
    ~@(map (fn [[_ val]]
             val)
           (partition 2 binds))))

#_(utils/mac
   (our-let [x 1
             y 1]
            (+ x y 1)))

; This throws an error because x will have no binding to pass to y.
; For this to work we'd need to create a new fn for each binding
#_(utils/mac
   (our-let [x 1
             y x]
            (+ x y 1)))

;;; Macros with bind variables

(defmacro when-bind [[var expr] & body]
  `(let [~var ~expr]
     (when ~var
       ~@body)))

#_(utils/mac (when-bind [x 1] (+ x 1)))

(defmacro when-bind*
  "Takes a list of pairs of the form (symbol expression) - the same form as the
   first argument to let. If any expression returns false, the whole when-bind*
   expression returns nil. Otherwise its body will be evaluated with each
   symbol bound as if by let."
  [binds & body]
  (if (empty? binds)
    `(do ~@body)
    `(let ~(vec (take 2 binds))
       (when ~(first binds)
         (when-bind* ~(drop 2 binds)
                     ~@body)))))

#_(utils/mac
   (when-bind* [x (some #(and (utils/consp %) %) '[a [1 2] b])
                y (some #(and (odd? %) %) x)]
               (+ y 10)))

(defmacro with-gensyms
  "Binds a whole list of variables to gensyms."
  [syms & body]
  `(let [~@(mapcat (fn [s]
                     `[~s (gensym)])
                   syms)]
     ~@body))

#_(utils/mac
   (with-gensyms [x y z]
     (list x y z)))

;;; Combination of cond and let

#_(def clauses '[[(= 1 2) [x (prn 'a)] [y (prn 'b)]]
                 [(= 1 1) [y (prn 'c)] [x (prn 'd)]]
                 [:else [x (prn 'a)] [z (prn 'f)]]])
#_(def cl '[(= 1 2) [x (prn 'a)] [y (prn 'b)]])
#_(def vars '{y G__8222, z G__8223, x G__8224})
#_(condlet-binds vars cl)

(defn condlet-binds
  "Bind the gensym vars to their respective expression"
  [vars cl]
  (mapcat (fn [[sym expr]]
            [(get vars sym) expr])
          (next cl)))

; (1) Bind the gensym vars to nil in case a vars was not set in the clause.
(defn condlet-clause
  "Returns a condlet clause. Also binds the vars to their respective 
   expressions."
  [vars cl bodfn]
  `[~(first cl) (let [~@(mapcat (fn [g] [g nil]) (vals vars))] ; (1)
                  (let [~@(condlet-binds vars cl)]
                    (~bodfn ~@(vals vars))))])

(defmacro condlet
  "Takes a vector of bindings clauses, followed by a body of code. Each of 
   the binding clauses is guarded by a test expression; the body of code will 
   be evaluated with the bindings specified by the first binding clauses 
   whose test expression returns true. Variables which occur in some clauses 
   and not others will be bound to nil if the successful clause does not 
   specify bindings for them."
  [clauses & body]
  (let [bodfn (gensym)
        vars (->> (mapcat rest clauses)
                  (map first)
                  set
                  (mapcat (fn [s] [s (gensym)]))
                  (apply hash-map))]
    `(letfn [(~bodfn [~@(keys vars)]
               ~@body)]
       (cond
         ~@(mapcat (fn [cl]
                     (condlet-clause vars cl bodfn))
                   clauses)))))

#_(utils/mac
   (condlet [[(= 1 2) [x (utils/princ 'a)] [y (utils/princ 'b)]]
             [(= 1 1) [y (utils/princ 'c)] [x (utils/princ 'd)]]
             [:else [x (utils/princ 'a)] [z (utils/princ 'f)]]]
            (list x y z)))

;;; ----------------------------------------------------------------------------
;;; 11.2 The with- Macro

(defmacro with-db [db- & body]
  `(let [temp# @db]
     (try
       (reset! db ~db-)
       (lock! @db)
       ~@body
       (catch Expression e (.getMessage e))
       (finally
         (release! db)
         (reset! db temp#)))))