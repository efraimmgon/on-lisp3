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

;;; ----------------------------------------------------------------------------
;;; 11.3 Conditional Evaluation

(defmacro if3
  "Three-valued logic. Instead of trating nil as false and eveything else as 
   true, this macro considers three categories of truth: true, false, and
   uncertain, represented as ?."
  [test t-case f-case ?-case]
  (case ~test
    (nil false) ~f-case
    ? ~?-case
    ~t-case))

(defmacro nif
  "Numeric if. Takes a numeric expression as its first argument and depending 
   on its sign evaluates one of the remaining three arguments."
  [expr pos zero neg]
  `(let [expr# ~expr]
     (cond (pos? expr#) ~pos
           (zero? expr#) ~zero
           :else ~neg)))

#_(map #(nif % :p :z :n) [0 1 -1])

(defmacro in
  "For testing efficiently for set membership."
  [obj & choices]
  (let [insym (gensym)]
    `(let [~insym ~obj]
       (or ~@(for [c choices]
               `(= ~insym ~c))))))

#_(utils/mac
   (in 2 1 2 3))

(defmacro inq
  "in queue. A quoting variant of in."
  [obj & args]
  `(in ~obj ~@(for [a args]
                `'~a)))


#_(utils/mac (inq 'a d c b a))

(defmacro in-if
  "Takes a predicate and returns true if any of the args tests positive."
  [pred & choices]
  (let [fnsym (gensym)]
    `(let [~fnsym ~pred]
       (or ~@(for [c choices]
               `(~fnsym ~c))))))

#_(utils/mac (in-if odd? 2 4 6 8 9))

(defn >casex [g cl]
  (let [[k then] cl]
    (cond (coll? k) `((in ~g ~@k) ~then)
          (inq k true :else) `(:else ~then)
          :else (throw (Exception. "bad >case clause")))))

; The > in the name is intended to suggest the arrow notation used to 
; represent evaluation.
; Since keys can be Lisp expressions, there is no way to tell if (x y) is a 
; call or a list of two keys. To avoid ambiguity, keys (other than true and 
; :else) must always be given in a list, even if there is only one of them.
(defmacro >case
  "Like `case`, but with keys which are evaluated before comparison - but it
   evalutated no more of the keys than it needs to. Keys must always be given
   in a list, even if there is only one of them."
  [expr & clauses]
  (let [g (gensym)]
    `(let [~g ~expr]
       (cond ~@(mapcat (fn [cl] (>casex g cl))
                       (partition 2 clauses))))))

#_(utils/mac
   (>case (+ 1 1)
          (:a :b :c) (prn 1)
          (3 (+ 1 1) 1) (prn 2)
          :else (prn 3)))

;;; ----------------------------------------------------------------------------
;;; 11.4 Iteration

(defmacro till
  "Converse of while. Loops while a test expression returns false."
  [test & body]
  (while (not test)
    ~@body))

(defmacro forn [[var start stop] & body]
  `(loop [~var ~start
          stop# ~stop]
     (when-not (> ~var stop#)
       ~@body
       (recur (inc ~var) stop#))))

(defmacro do-tuples|o
  "Evaluates its body with a tuple of variables bound to successive 
   subsequences of a sequence."
  [params source & body]
  (when (seq params)
    (let [src (gensym)]
      `(let [~src ~source]
         (utils/mapc (fn ~params ~@body)
                     ~@(map (fn [n]
                              `(nthnext ~src ~n))
                            (range (count params))))))))


#_(utils/mac (do-tuples|o [x y] '[a b c d]
                          (pr (list x y))))