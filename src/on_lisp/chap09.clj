(ns on-lisp.chap09
  (:require
   [on-lisp.utils :as utils]))

;;; ----------------------------------------------------------------------------
;;; 9
;;; Variable Capture
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; 9.1 Macro Argument Capture


(defmacro pfor [[var start stop] & body]
  `(loop [~var ~start
          ~'limit ~stop]
     (when (<= ~var ~'limit)
       ~@body
       (recur (inc ~var)
              ~'limit))))

#_(utils/mac
   (pfor [x 1 5] (pr x)))

; this will loop forever
#_(pfor [limit 1 5] (pr limit))

;;; ----------------------------------------------------------------------------
;;; 9.2 Free Symbol Capture
