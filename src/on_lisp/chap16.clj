(ns on-lisp.chap16
  (:require
   [on-lisp.utils :as utils]))

;;; ----------------------------------------------------------------------------
;;; 16
;;; Macro-Defining Macros
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; 16.1 Abbreviations

; I guess pg, being a vim user, never used a text editor with autocompletion.

(defmacro abbrev
  [short long]
  `(defmacro ~short [& args#]
     `(~'~long ~@args#)))

#_(utils/mac (abbrev != not=))
#_(utils/mac (!= 1 2))

(defmacro abbrevs
  [& names]
  `(do
     ~@(for [pair (partition 2 names)]
         `(abbrev ~@pair))))

#_(utils/mac (abbrevs != not=))

;;; ----------------------------------------------------------------------------
;;; 16.2 Properties

(defmacro color [x]
  `(get ~x :color))

(defmacro propmacro [propname]
  `(defmacro ~propname [~'obj]
     `(get ~~'obj '~'~propname)))

#_(utils/mac (propmacro color))
#_(utils/mac (color a))

(defmacro propmacros [& props]
  `(do
     ~@(for [p props]
         `(propmacro ~p))))

;;; ----------------------------------------------------------------------------
;;; 16.3 Anaphoric Macros

(defn a+expand [args syms]
  (if (seq args)
    (let [sym (gensym)]
      `(let [~sym ~(first args)
             ~'it ~sym]
         ~(a+expand (next args)
                    (concat syms (list sym)))))
    `(+ ~@syms)))

(defmacro a+
  "Anaphoric +. `it` is always bound to the value returned by the previous 
   argument."
  [& args]
  (a+expand args nil))

; This is an interesting macro, but not as useful in clj, since we have
; -> and as->, which are more elegant and flexible.
#_(utils/mac (a+ 7.95 (* it 0.05) (* it 3)))

(defn alist-expand [args syms]
  (if (seq args)
    (let [sym (gensym)]
      `(let [~sym ~(first args)
             ~'it ~sym]
         ~(alist-expand (next args)
                        (concat syms (list sym)))))
    `(list ~@syms)))

(defmacro alist
  "Anaphoric list. `it` is always bound to the valure returned by the previous 
   argument. The result of the expressions are returned in a list."
  [& args]
  (alist-expand args nil))

#_(alist 1 (+ 2 it) (+ 2 it))

(defn pop-symbol [sym]
  (->> sym str next (apply str) symbol))

(defn anaphex [args expr]
  (if (seq args)
    (let [sym (gensym)]
      `(let [~sym ~(first args)
             ~'it ~sym]
         ~(anaphex (next args)
                   (concat expr (list sym)))))
    expr))

(defn anaphex2 [op args]
  `(let [~'it ~(first args)]
     (~op ~'it ~@args)))

#_(defmacro aif [& args]
    (anaphex2 'if args))

(defmacro defanaph
  "Macro-defining macro. Creates an anaphoric variant of anything whose 
   arguments are evaluated according to the normal evaluation rule for 
   functions.
   By default, determines what to call in the expansion by pulling the first 
   letter (presumably an a) from the front of its argument. An alternate name 
   can be given as an optional argument. 
   The type of macro expansion desired can be signaled with the optional 
   `rule` keyword parameter, which specifies the evaluation to be used for 
   the arguments in the macro call:
   -> :all (the default) - all the arguments of the macro call will be 
   evaluated, with `it` always bound to the value of the previous argument.
   -> :first  - only the first argument will necessarily be evaluated, and 
   `it` will be bound to its value."
  [name & opts]
  (let [opts (apply hash-map opts)
        opname (or (:calls opts) (pop-symbol name))
        body (case (or (:rule opts) :all)
               :all `(anaphex ~'args '(~opname))
               :first `(anaphex2 '~opname ~'args))]
    `(defmacro ~name [& ~'args]
       ~body)))

(utils/mac (defanaph a+))
(utils/mac (defanaph alist))
(utils/mac (defanaph aif :rule :first))