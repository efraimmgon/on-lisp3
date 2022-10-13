(ns on-lisp.chap04)

;;; ----------------------------------------------------------------------------
;;; 4
;;; Utility Functions
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Operations on lists

(defn single?
  "Returns true if the collection contains one element."
  [coll]
  (and (seq coll)
       (not (next coll))))

#_(single? [])
#_(single? [1])
#_(single? [1 2])

(defn append1
  "Attachs a new element to the end of the list."
  [coll obj]
  (concat coll (list obj)))

#_(append1 [] 1)
#_(append1 [1 2] 3)

(defn mklist
  "Ensures that obj is returned as a collection."
  [obj]
  (if (coll? obj)
    obj
    (list obj)))

#_(mklist 1)
#_(mklist [1])

(defn mkvec
  "Ensures that obj is returned as a collection."
  [obj]
  (if (coll? obj)
    obj
    [obj]))

#_(mkvec 1)
#_(mkvec [1])

(defn longer?
  "Compares the length of two collections and returns true if the first 
   is longer."
  [x y]
  (letfn [(compare [x y]
            (and (seq x)
                 (or (empty? y)
                     (compare (rest x) (rest y)))))]
    (if (and (seqable? x) (seqable? y))
      (compare x y)
      (> (count x) (count y)))))

#_(longer? [1] [1 2])
#_(longer? [1 2] [3])

;; Same as `partition-all`.
(defn group
  "Returns a sequence of lists of n elements."
  [source n]
  (if (zero? n)
    (throw (Exception. "zero length."))
    (letfn [(rec [source acc]
              (let [more (nthrest source n)]
                (if (seq more)
                  (recur more
                         (cons (take n source) acc))
                  (reverse (cons source acc)))))]
      (when (seq source)
        (rec source nil)))))

#_(group [1 2 3 4 5 6 7] 2)

;; Same as `flatten`.
(defn flatten-
  [x]
  (letfn [(rec [x acc]
            (cond (nil? x) acc
                  (not (coll? x)) (cons x acc)
                  :else (rec (first x)
                             (rec (next x) acc))))]
    (rec x nil)))

#_(flatten- [1 [2 3] [[4 5] 6]])

(defn prune
  "Returns a sequence with every leaf for wich the test function returns 
   true removed."
  [test tree]
  (letfn [(rec [tree acc]
            (cond (nil? tree) (reverse acc)

                  (coll? (first tree))
                  (rec (next tree)
                       (cons (rec (first tree) nil)
                             acc))

                  :else (rec (next tree)
                             (if (test (first tree))
                               acc
                               (cons (first tree) acc)))))]
    (rec tree nil)))

#_(prune even? [1 2 [3 [4 5] 6] 7 8 [9]])

;;; ----------------------------------------------------------------------------
;;; Search

(defn find2
  "Returns the a vec of the element and (f elt) of the first occurrence 
   that returns true."
  [f lst]
  (when (seq lst)
    (let [val (f (first lst))]
      (if val
        [(first lst) val]
        (recur f (rest lst))))))

#_(find2 even? [1 3 5 2 9 8])

(defn before
  "Returns true if x is found before y in the sequence."
  [x y lst]
  (and (seq lst)
       (let [a (first lst)]
         (cond (= y a) nil
               (= x a) lst
               :else (recur x y (rest lst))))))

#_(before 1 4 [1 2 3 4])
#_(before 1 2 [1])
#_(before 2 1 [1 2 3 4])

(defn member
  "Returns true if obj is in lst. Same as CL member."
  [obj lst]
  (loop [lst lst]
    (when (seq lst)
      (if (= obj (first lst))
        lst
        (recur (rest lst))))))

(defn after
  "Returns true if x is found after y in the sequence."
  [x y lst]
  (when-let [more (before y x lst)]
    (member x more)))

#_(after 2 1 [1 2 3])
#_(after 1 2 [1])

(defn duplicate
  "Returns true if obj occurs more than once in the sequence."
  [obj lst]
  (->> (member obj lst)
       rest
       (member obj)))

#_(duplicate 1 [1 2 3 1 4])

;; Similar, but different from `split-with`
(defn split-if
  "Returns a list of two sequences split where f first returns true."
  [f lst]
  (loop [acc nil
         src lst]
    (if (or (empty? src) (f (first src)))
      [(reverse acc) src]
      (recur (cons (first src) acc)
             (rest src)))))

#_(split-if #(> % 4) (range 10))
#_(split-with (partial > 4) (range 10))
#_(split-with #(> % 4) (range 10))