(ns on-lisp.chap10
  (:require
   [on-lisp.utils :as utils]))

;;; ----------------------------------------------------------------------------
;;; 10
;;; Other Macro Pitfalls
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; 10.1 Number of Evaluations

;; A correct version:
(defmacro our-for [[var start stop] & body]
  `(loop [~var ~start
          stop# ~stop]
     (when-not (> ~var stop#)
       (recur (inc ~var) stop#))))

;; Subject to multiple evaluations:
#_(defmacro our-for [[var start stop] & body]
    `(loop [~var ~start]
       (when-not (> ~var ~stop)
         (recur (inc ~var)))))

;; Incorrect order of evaluation:
#_(defmacro our-for [[var start stop] & body]
    `(loop [stop# ~stop
            ~var ~start]
       (when-not (> ~var stop#)
         (recur (inc ~var) stop#))))


