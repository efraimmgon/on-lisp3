(ns on-lisp.chap13
  (:require
   [on-lisp.utils :as utils]))

;;; ----------------------------------------------------------------------------
;;; 13
;;; Computation at Compile-Time
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; 13.1 New Utilities

(defn avg-fn [& args]
  (/ (apply + args)
     (count args)))

(defmacro avg [& args]
  `(/ (+ ~@args)
      ~(count args)))

(defn most-of-fn
  "Returns true if most of its arguments do"
  [& args]
  (let [[all hits]
        (reduce (fn [[all hits] a]
                  [(inc all) (if a (inc hits) hits)])
                [0 0] args)]
    (> hits (/ all 2))))

(defmacro most-of
  "Returns true if most of its arguments do"
  [& args]
  (let [need (-> (count args) (/ 2) Math/floor)
        hits (gensym)]
    `(let [~hits (atom 0)]
       (or ~@(map (fn [a]
                    `(and ~a
                          (> (swap! ~hits inc)
                             ~need)))
                  args)))))

#_(most-of-fn true true true nil nil)
#_(utils/mac (most-of true true true nil nil))

(defn nthmost
  "Takes a number n and a list of numbers, and returns the nth largest 
   among them."
  [n lst]
  (nth (sort > lst) n))

(comment
  ;; will have to change the how expr to replace setq here

  (defn nthmost-gen [var vars & [long?]]
    (when (seq vars)
      (let [else (nthmost-gen var (rest vars) long?)]
        (if (and (not long?) (empty? else))
          `(setq ~(first vars) ~var) ; replace setq
          `(if (> ~var ~(first vars))
             (setq ~@(mapcat list ; replace setq
                             (reverse vars)
                             (rest (reverse vars)))
                   ~(first vars) ~var)
             ~else)))))

  (defn gen-start [glst syms]
    (->> (reverse syms)
         (utils/maplist (fn [syms]
                          (let [var (gensym)]
                            `(let [~var (first (deref ~glst))
                                   _ (swap! ~glst next)]
                               ~(nthmost-gen var (reverse syms))))))
         reverse))

  (defmacro nthmost
    "Takes a number n and a list of numbers, and returns the nth largest 
   among them."
    [n lst]
    (if (and (integer? n) (< n 20))
      (let [glst (gensym)
            gi (gensym)
            syms (map (fn [_] (gensym))
                      (range 0 (inc n)))]
        `(let [~glst (atom ~lst)]
           (when-not (< (count ~glst)
                        ~(inc n))
             ~@(gen-start glst syms)
             (doseq [~gi (deref ~glst)]
               ~(nthmost-gen gi syms true))
             ~((first (last syms))))))
      `(nth (sort > ~lst) ~n))))

#_(nthmost 2 '(2 6 1 5 3 4))