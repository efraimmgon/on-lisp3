(ns on-lisp.chap09
  (:require
   [on-lisp.utils :as utils]))

;;; ----------------------------------------------------------------------------
;;; 9
;;; Variable Capture
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; 9.1 Macro Argument Capture

;; Instances of variable capture can be traced to one of two situations: macro 
;; argument capture and free symbol capture. In argument capture, a symbol  
;; passed as an argument in the macro call inadvertently refers to a variable  
;; established by the macro expansion itself.

;; Note: In Clojure the backquote will fully namespace the symbols inside of it.
;; In this example, this would lead to an error, since one can only use 
;; simple symbols for bindings. To get the same result as the book's example
;; we would have to get out of our way to make `limit` a simple symbol.
;; The idiomatic way to do this is to use a gensym `limit#`.

(defmacro pfor [[var start stop] & body]
  `(loop [~var ~start
          ~'limit ~stop] ; in this case, `limit`
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

;; Less frequently, the macro definition itself contains a symbol which 
;; inadvertently refers to a binding in the environment where the macro is 
;; expanded.

;; Note: same as 9.1 regarding Clojure.

(def w (atom nil))

; We must go out of our way to make `w` a simple symbol.
(defmacro gripe [warning]
  `(do (swap! ~'w conj ~warning)
       nil))

(defn sample-ratio [v w]
  (let [vn (count v)
        wn (count w)]
    (if (or (< vn 2) (< wn 2))
      (gripe "sample < 2")
      (/ vn wn))))

; This throws an error
#_(utils/mac (let [lst [:b]]
               (sample-ratio nil lst)))