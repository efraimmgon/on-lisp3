(ns on-lisp.chap08
  (:require
   [on-lisp.utils :as utils]))

;;; ----------------------------------------------------------------------------
;;; 8
;;; When to Use Macros
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; 8.2 Macro or Function?

(defn avg [& args]
  (float
   (/ (apply + args)
      (count args))))

(defmacro avg [& args]
  `(float
    (/ (+ ~@args)
       ~(count args))))

