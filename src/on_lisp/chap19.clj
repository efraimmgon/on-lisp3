(ns on-lisp.chap19
  (:require
   [on-lisp.utils :as utils]))

;;; ----------------------------------------------------------------------------
;;; 19
;;; A Query Compiler
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; 19.1 The Database

(defn  make-db
  "Creates a new database."
  []
  (atom {}))

; It is up to the user to decide how to organize the database. The only
; restriction is that the entries (facts) will be indexec under their first
; element (the predicate).
(def default-db
  "The default database."
  (make-db))

(defn clear-db
  "Clears the default database."
  []
  (reset! default-db {}))

(defmacro db-query
  "Returns the facts in the database with the given predicate."
  [pred & [database]]
  `(let [db# (or ~database default-db)]
     (get @db# ~pred)))

(defn db-push
  "Adds a fact to the database."
  ([k v]
   (db-push default-db k v))
  ([db k v]
   (swap! db update k conj v)))

; The macro fact adds a new fact to the dabase
(defmacro fact
  "Adds a fact to the database."
  [pred & args]
  `(do
     (db-push '~pred '~args)
     '~args))

(comment
  @default-db
  (fact painter reynolds joshua english)
  (fact painter canale antonio venetian)
  (utils/mac (db-query 'painter)))

;;; ----------------------------------------------------------------------------
;;; 19.2 Pattern-Matching Queries

; Syntax of queries:
; <query>    : (<symbol> <argument>*) 
;            : (not <query>)
;            : (and <query>*)
;            : (or <query>*)
; <argument> : ?<symbol>
;            : <symbol>
;            : <number>  