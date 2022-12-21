(ns on-lisp.chap19
  (:require
   [on-lisp.chap18 :as chap18]
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

(fact painter reynolds joshua english)
(fact painter canale antonio venetian)
#_(utils/mac (db-query 'painter))

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

;;; ----------------------------------------------------------------------------
;;; 19.3 A Query Interpreter

(defn lookup
  "Takes a pattern consisting of a predicate and a list of arguments and
   returns a list of bindings that match some fact in the database. It gets
   all the database entries for the predicate, and calls match to compare each
   of them against the pattern."
  ([pred args]
   (lookup pred args {}))
  ([pred args binds]
   (mapcat (fn [x]
             (when-let [it (chap18/match x args binds)]
               (list it)))
           (db-query pred))))

#_(lookup 'painter '(?x ?y english))

(declare interpret-query)

(defn interpret-not
  "The not operator is implemented by calling interpret-query recursively
   on the argument, and then filtering out the bindings that match."
  [clause binds]
  (when (interpret-query clause binds)
    (list binds)))

(defn interpret-or
  "The or operator is implemented by calling interpret-query recursively
   on each of the arguments, and then concatenating the results."
  [clauses binds]
  (mapcat (fn [c]
            (interpret-query c binds))
          clauses))

(defn interpret-and
  "The and operator is implemented by calling interpret-query recursively
   on each of the arguments, and then filtering out the bindings that do not
   match."
  [clauses binds]
  (if (empty? clauses)
    (list binds)
    (mapcat (fn [b]
              (interpret-query (first clauses) b))
            (interpret-and (next clauses) binds))))

#_(interpret-and '((dates ?fname 1697 ?death) (painter ?fname ?mname ?lname)) {})
#_(interpret-query '(dates ?fname 1697 ?death) {})
#_(interpret-query '(painter ?x ?y ?z) {})

(defn interpret-query
  "The query interpreter is implemented as a recursive function. It takes
   as its first argument a query, and as its second argument a list of
   bindings. It returns a list of bindings that match the query."
  ([expr]
   (interpret-query expr {}))
  ([expr binds]
   (case (first expr)
     and (interpret-and (reverse (next expr)) binds)
     or (interpret-or (next expr) binds)
     not (interpret-not (fnext expr) binds)
     (lookup (first expr) (next expr) binds))))

#_(interpret-query
   '(and (painter ?fname ?mname ?lname)
         (dates ?fname 1697 ?death)))

(comment
  "Provides a clean way of using the query interpreter. It takes as its first
   argument any legal query; the rest of the arguments are treated as a body
   of code.")
(defmacro with-answer
  [query & body]
  (let [binds (gensym)]
    `(doseq [~binds (interpret-query '~query)]
       (let [~@(mapcat (fn [v]
                         `(~v (chap18/bindings '~v ~binds)))
                       (chap18/vars-in query utils/atom?))]
         ~@body))))

; Assertion of sample facts
(clear-db)
(fact painter reynolds joshua english)
(fact painter canale antonio venetian)
(fact painter hogarth william english)
(fact dates hogarth 1697 1772)
(fact dates canale 1697 1768)
(fact dates reynolds 1723 1792)

; The first name and nationality of every painter called Hogarth.
(with-answer (painter hogarth ?x ?y)
  (println ?x ?y))

; The last name of eery painter born in 1697.
