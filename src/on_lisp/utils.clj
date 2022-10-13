(ns on-lisp.utils)

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

#_(atom? nil)
#_(atom? '())
#_(atom? [])
#_(atom? 1)
#_(atom? [1])