(ns on-lisp.chap06)

;;; ----------------------------------------------------------------------------
;;; 6
;;; Functions as Representation
;;; ----------------------------------------------------------------------------

;; The contents field will contain either a question or a return value. If 
;; the node is not a leaf, the yes and no fields will tell where to go 
;; depending on the answer to the question; if the node is a leaf, we will 
;; know it because these fields are empty.
(defrecord Node [contents yes no])

(def nodes
  "A hash-table in which nodes are indexed by name."
  (atom {}))

(defn defnode
  "Makes a new node (of either type) and stores it in nodes."
  [name conts & [yes? no?]]
  (swap! nodes
         assoc name
         (map->Node
          {:contents conts
           :yes yes?
           :no no?})))

(defn run-node
  "Given a name, we look up the corresponding node. If it is not a leaf, the 
   contents are asked as a question, and depending on the answer, we continue 
   traversing at one of two possible destinations. If the node is a leaf, 
   run-node just returns its contents."
  [name]
  (let [n (get @nodes name)]
    (cond (:yes n)
          (do
            (println (:contents n))
            (case (read)
              yes (run-node (:yes n))
              (run-node (:no n))))

          :else
          (:contents n))))

#_(run-node 'people)

;;; 6.5: A network compiled into closures

(defn defnode
  [name conts & [yes? no?]]
  (let [val (if yes?
              (fn []
                (println conts)
                (case (read)
                  yes ((get @nodes yes?))
                  ((get @nodes no?))))
              (fn [] conts))]
    (swap! nodes
           assoc name val)))

(defn reset-nodes []
  (reset! nodes {})
  (defnode 'people "Is the person a man?" 'male 'female)
  (defnode 'male "Is he living?" 'liveman 'deadman)
  (defnode 'deadman "Was he American?" 'us 'them)
  (defnode 'us "Is he on a coin?" 'coin 'cidence)
  (defnode 'coin "Is the coin a penny?" 'penny 'coins)
  (defnode 'penny 'lincoln))


#_(reset-nodes)
#_((get @nodes 'people))

;;; Compilation with static references.

(def nodes
  (atom {}))

(defn defnode
  "Makes a new node (of either type) and stores it in nodes."
  [name conts & [yes? no?]]
  (swap! nodes
         assoc name
         (map->Node
          {:contents conts
           :yes yes?
           :no no?})))

(defn compile-net
  "Recursively works its way right down to the leaves of the tree, and on the 
   way back up, returns at each step the node/function for each of the two 
   subtrees. So now each node will have a direct handle on its two 
   destinations, instead of having only their names. When the original call 
   to compile-net returns, it will yield a function representing the portion 
   of the network we asked to have compiled."
  [root]
  (when-let [node (get @nodes root)]
    (if-let [yes (:yes node)]
      (let [yes-fn (compile-net yes)
            no-fn (compile-net (:no node))]
        (fn []
          (println (:contents node))
          ((if (= (read) 'yes)
             yes-fn
             no-fn))))
      (fn [] (:contents node)))))

#_(reset-nodes)
#_(def n (compile-net 'people))
#_(n)