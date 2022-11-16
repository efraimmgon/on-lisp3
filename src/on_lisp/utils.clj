(ns on-lisp.utils
  (:require
   clojure.pprint))

(defn consp
  "Returns true if object is of type cons; otherwise, returns false."
  [lst]
  (and (coll? lst)
       (seq lst)))

(defn atom?
  "Returns true if obj is not seqable, except if it is nil or empty."
  [x]
  (if (seqable? x)
    (or (nil? x)
        (empty? x)
        false)
    true))

(defmacro mac
  "Pretty printing for macroexpand-1."
  [expr]
  `(clojure.pprint/pprint
    (macroexpand-1 '~expr)))


#_(atom? nil)
#_(atom? '())
#_(atom? [])
#_(atom? 1)
#_(atom? [1])

(defmacro with-gensyms
  "Binds a whole list of variables to gensyms."
  [syms & body]
  `(let [~@(mapcat (fn [s]
                     `[~s (gensym)])
                   syms)]
     ~@body))

(defn princ [x]
  (pr x)
  x)

(defn mapc
  "Like map, except that the results of applying f are not accumulated. 
   The list argument is returned."
  [f & seqs]
  (when (seq seqs)
    (loop [remain seqs]
      (if (some empty? remain)
        (first seqs)
        (do (apply f (map first remain))
            (recur (map next remain)))))))

(comment
  (def dummy (atom nil))
  (mapc (fn [& x] (swap! dummy concat x))
        '[1 2 3 4]
        '[a b c d e]
        '[x y z]))

(defn maplist
  "Like map, except that f is applied to successive subsequences of the 
   sequences. f is first applied to the sequences themselves, and then to the 
   rest of each sequence, and then to the rest of the rest of each sequence, 
   and so on."
  [f & seqs]
  (when-let [s (seq seqs)]
    (lazy-seq
     (when-not (some empty? s)
       (cons (apply f s)
             (apply maplist f
                    (map rest s)))))))

#_(maplist concat [1 2 3 4] [1 2] [1 2 3])