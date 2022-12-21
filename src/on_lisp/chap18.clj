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
  "Returns a vector of bindings for the given pattern and sequence. The  
   bindings are in the order they
  appear in the pattern. The optional `atom?` predicate determines whether a
  pattern is an atom. The optional `n` is the index of the first element in
  the sequence to be matched."
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

#_(destruc '(?x (?z & ?y) & ?x) '((a b) (1 2 3) a b))

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
  "Takes a pattern and a coll, and establishes bindings by comparing them. 
   Also takes a then clause to be evaluated, with new bindings, if the match 
   succeeds; and an else clause to be evaluated if the match fails."
  [pat coll then & [else]]
  (let [gbindings (gensym)]
    `(if-let [~gbindings (match '~pat '~coll)]
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
  "Returns true if `x` is a simple pattern element. A simple element is either 
  an atom or a list whose first element is `quote`."
  [x]
  (or (utils/atom? x)
      (= (first x) 'quote)))

#_(simple? 'x)
#_(simple? (gensym))
#_(simple? '(x y z))
#_(simple? '(quote x))

; (1) Unlike the original version, we need to use partition because `more`
; is a flat sequence, not a list of lists.
(defn length-test
  "Returns a test for the length of a pattern. The test is either 
  (= (count pat) (count more)) or (= (count pat) (- (count more) 2)). 
  The latter is used when the last element of more is a sequence or 
  a call to nth."
  [pat more]
  (let [fin (-> more last first)
        more-len (count (partition 2 more))] ; (1)
    (if (or (coll? fin) (= fin `nth))
      `(= (count ~pat) ~more-len)
      `(> (count ~pat) ~(- more-len 2)))))

#_(def more
    '[?x (clojure.core/nth G__9175 0)
      (quote a) (clojure.core/nth G__9175 1)])
#_(length-test 'a more)

(defn gensym?
  "Returns true if `s` is a gensym, false otherwise. A gensym is a symbol 
  whose name starts with the string \"G__\"."
  [s]
  (and (symbol? s)
       (clojure.string/starts-with? (name s) "G__")))

; This function considers four cases.
; (1) If the pattern argument is a gensym, then it is one of the invisible 
; variables created by destruct to hold sublists, and all we need to do at 
; runtime is test that it has the right length. (2) If the pattern is a wildcard
; (_), no code need be generated. (3) If the pattern is a variable, match1 
; generates code to match it against, or set it to, the corresponding part 
; of the sequence given at runtime. (4) Otherwise, the pattern is taken to be
; a literal value, and match1 generates code to compare it with the 
; corresponding part of the sequence.
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
  (dbind [[pat expr & more] refs]
               ; (1)
         (cond (gensym? pat)
               `(let [~pat ~expr]
                  (if (and (coll? ~pat)
                           ~(length-test pat more))
                    ~then
                    ~else))
               ; (2)
               (= pat '_) then
               ; (3)
               (varsym? pat)
               (let [ge (gensym)]
                 `(let [~ge ~expr]
                    (if (or (gensym? ~pat) (= ~pat ~ge))
                      (let [~pat ~ge]
                        ~then)
                      ~else)))
               ; (4)
               :else `(if (= ~pat ~expr) ~then ~else))))



#_(match1 '((?x ?y ?x ?y) '(h1 ho h1 ho))
          '[?x ?y]
          nil)

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
  "Takes a pattern, a coll, a then clause, and an else clause. 
   The pattern is a pattern variable, a literal, or a pattern structure. 
   The coll is an expression to be matched against the pattern. 
   The then clause is evaluated if the match succeeds, and the else clause 
   is evaluated if the match fails. The then clause is evaluated with 
   bindings established by the match. The else clause is evaluated with 
   no bindings."
  [pat coll then else]
  (if (simple? pat)
    (match1 `((~pat ~coll)) then else)
    (let [gseq (gensym)
          gelse (gensym)]
      `(letfn [(~gelse [] ~else)]
         ~(gen-match (into [gseq coll]
                           (destruc pat gseq :atom? simple?))
                     then
                     `(~gelse))))))

(defmacro if-match
  "Takes a pattern and a coll, and establishes bindings by comparing them. 
   Also takes a then clause to be evaluated, with new bindings, if the match 
   succeeds; and an else clause to be evaluated if the match fails."
  [pat coll then & [else]]
  `(let [~@(mapcat (fn [v]
                     `(~v '~(gensym)))
                   (vars-in pat simple?))]
     (pat-match ~pat ~coll ~then ~else)))

#_(utils/mac (if-match (?x ?y ?x ?y) '(h1 ho h1 ho)
                       [?x ?y]
                       nil))
#_(utils/mac
   (if-match (?x ?y ?x ?y) [:h1 :ho :h1 :ho]
             [?x ?y]
             nil))
#_(utils/mac
   (if-match (?x (1 & ?y) & ?x) '((a b) (1 2 3) a b)
             [?x ?y]))
#_(utils/mac
   (pat-match (?x (?z & ?y) & ?x) '((a b) (1 2 3) a b)
              [?x ?y]
              nil))
#_(utils/mac
   (pat-match (?x & ?y) '(1 2 3)
              [?x ?y]
              nil))

#_(utils/mac
   (if-match (?x 2 & ?y) '(1 2 3 4)
             [?x ?y]
             nil))