(ns on-lisp.chap02)

;;; ----------------------------------------------------------------------------
;;; 2
;;; Functions
;;; ----------------------------------------------------------------------------

(defn make-adder [n]
  (fn [x] (+ x n)))

#_(let [add10 (make-adder 10)]
    (add10 3))

;;; Making it possible to change the value of a closure
(defn make-adderb [n]
  (let [n (atom n)]
    (fn [x & [change]]
      (if change
        (reset! n x)
        (+ x @n)))))

(comment
  (def addx (make-adderb 1))
  (addx 3)
  (addx 100 true)
  (addx 3))

