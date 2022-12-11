(ns on-lisp.chap18
  (:require
   clojure.set
   clojure.string
   [on-lisp.utils :as utils :refer [acond aif]]))

;;; ----------------------------------------------------------------------------
;;; 18
;;; Destructuring
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; 18.2 Other Structures

(defn dbind-ex [binds body]
  (if (empty? binds)
    `(do ~@body)
    `(let [~@(mapcat (fn [b]
                       (if (coll? (first b))
                         (first b)
                         b))
                     (partition 2 binds))]
       ~(dbind-ex (mapcat (fn [b]
                            (when (coll? (first b))
                              (next b)))
                          (partition 2 binds))
                  body))))

; NOTE: our `destruc` returns a flat vector with the bindings in the clj style,
; which differs from the original that returns a list with lists of bindings,
; in the CL style.
; NOTE: takes two optional keys: `atom?` and `n`. `atom?` is a predicate that
; determines whether a pattern is an atom. `n` is the index of the first
; element in the sequence to be matched.
(defn destruc
  [pat sequence & kwargs]
  (let [{:keys [atom? n] :or {atom? utils/atom?, n 0}}
        (apply hash-map kwargs)]
    (when (seq pat)
      (let [more (cond (atom? pat) pat
                       (= (first pat) '&) (fnext pat)
                       :else nil)]
        (if more
          `(~more (subvec (vec ~sequence) ~n))
          (let [p (first pat)
                rec (destruc (next pat) sequence :atom? atom? :n (inc n))]
            (if (atom? p)
              (into `[~p (nth ~sequence ~n)]
                    rec)
              (let [var (gensym)]
                (into
                 (into `[~var (nth ~sequence ~n)]
                       (destruc p var :atom? atom?))
                 rec)))))))))

#_(destruc '[a b c] [1 2 3])
#_(destruc '[a b & c] [1 2 3])
#_(destruc '[a [b c] d] [1 [2 3] 4])

; Resembles `destructuring-bind`, but works for any kind of sequence. The 
; second argument can be alist, a vector, or any combination thereof.
(defmacro dbind
  [[pat sequence] & body]
  (let [gseq (gensym)]
    `(let [~gseq ~sequence]
       ~(dbind-ex (destruc pat gseq :atom? utils/atom? :n 0)
                  body))))

#_(utils/mac (dbind [[a b c]
                     [1 2 3]]
                    (list a b c)))
#_(utils/mac (dbind [a [b c] d] [1 [2 3] 4]
                    (list a b c d)))

#_(utils/mac (dbind [a [b & c] & d] '[1 "fribble" 2 3 4]
                    (list a b c d)))

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

(defn bindings
  "Returns the binding associated with `x`."
  [x binds]
  (when-let [val (get binds x)]
    (or (bindings val binds)
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

    (bindings x binds)
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

(defn vars-in
  "Returns a set of all the pattern variables in an expression."
  ([expr]
   (vars-in expr utils/atom?))
  ([expr atom?]
   (if (atom? expr)
     (when (varsym? expr)
       #{expr})
     (clojure.set/union
      (vars-in (first expr) atom?)
      (vars-in (next expr) atom?)))))

#_(vars-in '(?x ?y z))
#_(vars-in '(?x (?y (?z))))

(defmacro if-match-v0
  "Takes a pattern and a sequence, and establishes bindings by comparing them. 
   Also takes a then clause to be evaluated, with new bindings, if the match 
   succeeds; and an else clause to be evaluated if the match fails."
  [pat sequ then & [else]]
  (let [gbindings (gensym)]
    `(if-let [~gbindings (match '~pat '~sequ)]
       (let [~@(mapcat (fn [v]
                         `(~v (bindings '~v ~gbindings)))
                       (vars-in then utils/atom?))]
         ~then)
       ~else)))

#_(utils/mac (if-match-v0 (?x ?y ?x ?y) (h1 ho h1 ho)
                          [?x ?y]
                          nil))

; Defines the distinction between pattern content and pattern structure. If 
; we want to use quoted literals in patterns, the destructuring code 
; (and vars-in) have to be told not to go inside lists whose first element is 
; quote. With the new matching operator, we will be able to use lists as 
; pattern elements, simply by quoting them.
(defn simple?
  [x]
  (or (utils/atom? x)
      (= (first x) 'quote)))

(defn length-test
  "Returns a test for the length of a pattern. The test is either 
  (= (count pat) (count rest)) or (= (count pat) (- (count rest) 2)). 
  The latter is used when the last element of rest is a sequence or 
  a call to nth."
  [pat rest]
  (let [fin (-> rest last ffirst)]
    (if (or (coll? fin) (= fin 'nth))
      `(= (count ~pat) ~(count rest))
      `(= (count ~pat) ~(- (count rest) 2)))))

(defn gensym?
  "Returns true if `s` is a gensym, false otherwise. A gensym is a symbol 
  whose name starts with the string \"G__\"."
  [s]
  (and (symbol? s)
       (clojure.string/starts-with? "G__" (name s))))


(defn match1
  "Takes a list of references, a then clause, and an else clause. 
  The references are a sequence of pairs of pattern and expression. 
  The pattern is a pattern variable, a literal, or a pattern structure. 
  The expression is an expression to be matched against the pattern. 
  The then clause is evaluated if the match succeeds, and the else clause 
  is evaluated if the match fails. The then clause is evaluated with 
  bindings established by the match. The else clause is evaluated with 
  no bindings."
  [refs then else]
  (dbind [[pat expr & rest] refs]
         (cond (gensym? pat)
               `(let [~pat ~expr]
                  (if (and (coll? ~pat)
                           ~(length-test pat rest))
                    ~then
                    ~else))

               (= pat '_) then

               (varsym? pat)
               (let [ge (gensym)]
                 `(let [~ge ~expr]
                    (if (or (gensym? ~pat) (= ~pat ~ge))
                      (let [~pat ~ge]
                        ~then)
                      ~else)))

               :else `(if (= ~pat ~expr) ~then ~else))))

"Returns a match expression for a sequence of references, using match1."
(defn gen-match [refs then else]
  (if (empty? refs)
    then
    (let [then (gen-match (drop 2 refs) then else)]
      (if (simple? (first refs))
        (match1 refs then else)
        (gen-match (take 2 refs) then else)))))

; Takes the same arguments as if-match; the only difference is that it 
; establishes no new bindings for pattern variables.
(defmacro pat-match
  "Takes a pattern, a sequence, a then clause, and an else clause. 
   The pattern is a pattern variable, a literal, or a pattern structure. 
   The sequence is an expression to be matched against the pattern. 
   The then clause is evaluated if the match succeeds, and the else clause 
   is evaluated if the match fails. The then clause is evaluated with 
   bindings established by the match. The else clause is evaluated with 
   no bindings."
  [pat sequence then else]
  (if (simple? pat)
    (match1 `((~pat ~sequence)) then else)
    (let [gseq (gensym)
          gelse (gensym)]
      `(letfn [(~gelse [] ~else)]
         ~(gen-match (into [gseq sequence]
                           (destruc pat gseq :atom? simple?))
                     then
                     `(~gelse))))))

(defmacro if-match
  "Takes a pattern and a sequence, and establishes bindings by comparing them. 
   Also takes a then clause to be evaluated, with new bindings, if the match 
   succeeds; and an else clause to be evaluated if the match fails."
  [pat sequence then & [else]]
  `(let [~@(mapcat (fn [v]
                     `(~v '~(gensym)))
                   (vars-in pat simple?))]
     (pat-match ~pat ~sequence ~then ~else)))

#_(utils/mac (if-match (?x ?y ?x ?y) (h1 ho h1 ho)
                       [?x ?y]
                       nil))
; the same as above, but with pat-match
#_(utils/mac (pat-match (?x ?y ?x ?y) (h1 ho h1 ho)
                        [?x ?y]
                        nil))
#_(utils/mac
   (clojure.core/let
    [?x 'G__8306 ?y 'G__8307]
     (on-lisp.chap18/pat-match (?x ?y ?x ?y) (h1 ho h1 ho) [?x ?y] nil)))