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