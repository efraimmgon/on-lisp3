(ns on-lisp.chap03)

;;; ----------------------------------------------------------------------------
;;; 3
;;; Functional Programming
;;; ----------------------------------------------------------------------------

(defn good-reverse [lst]
  (letfn [(rev [lst acc]
            (if (seq lst)
              (rev (rest lst) (cons (first lst) acc))
              acc))]
    (rev lst nil)))

#_(good-reverse [1 2 3])
