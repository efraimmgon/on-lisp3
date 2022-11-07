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

#_(defmacro pfor [[var start stop] & body]
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

;;; ----------------------------------------------------------------------------
;;; 9.3 When Capture Occurs

(comment
  "Free: A symbol s occurs free in a expression when it is used as a variable 
   in that expression, but the expression does not create a binding for it."

  "Skeleton: The skeleton of a macro expansion is the whole expansion, minus 
   anything which was part of an argument in the macro call."

  "Capturable: A symbol is capturable in some macro expansion if (a) it occurs 
   free in the skeleton of the macro expansion, or (b) it is bound by a part 
   of the skeleton in which arguments passed to the macro are either bound 
   or evaluated.")

;;; ----------------------------------------------------------------------------
;;; 9.5 Avoiding Capture by Prior Evaluation

; Note: this exemple does not apply to Clojure because of 

;;; ----------------------------------------------------------------------------
;;; 9.6 Avoiding Capture with Gensyms

;; Vulnerable to capture:
#_(defmacro pfor [[var start stop] & body]
    `(loop [~var ~start
            ~'limit ~stop]
       (when (<= ~var ~'limit)
         ~@body
         (recur (inc ~var)
                ~'limit))))

;; A correct version:
(defmacro pfor [[var start stop] & body]
  `(loop [~var ~start
          stop# ~stop]
     (when (<= ~var stop#)
       ~@body
       (recur (inc ~var)
              stop#))))
